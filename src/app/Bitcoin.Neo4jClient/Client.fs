namespace BitcoinFs.Neo4jClient
open System
open System.Linq
open BitcoinFs
open Neo4jClient

[<CLIMutable>]
type NeoInput = 
    { Hash: string
      Index: int
      Value: int64
      Address: string }

[<CLIMutable;>]
type NeoOutput = 
    { Value: int64
      Address: string
      Index: int }

[<CLIMutable>]
type NeoTransaction = 
    { TransactionHash: string
      TotalInputs: int64
      TotalOutputs: int64 }

[<CLIMutable>]
type NeoBlock = 
    { Hash: string
      Timestamp: DateTimeOffset } 

[<CLIMutable>]
type NeoAddress = 
    { Address: string
      Balance: int64 }

module LoadBlockChainModel =
    let client = new GraphClient(new Uri("http://localhost:7474/db/data"))
    client.Connect()

    // TODO feels like we could have a generic save method
    let saveNeoOutput transactionHash neoOutput =
        client.Cypher
            .Create("(o:Output {param})")
            .WithParam("param", neoOutput)
            .With("o")
            .Match("(t:Transaction)")
            .Where(fun t -> t.TransactionHash = transactionHash)
            .CreateUnique("t-[:output]->o")
            .Return<NeoOutput>("o")
            .Results
            .Single()

    let payToAddress transactionHash address value =
        client.Cypher
            .Merge("(a:Address { Address: {param}})")
            .WithParam("param", address)
            .OnCreate()
            .Set("a.Balance = {balance}")
            .OnMatch()
            .Set("a.Balance = a.Balance + {balance}")
            .With("a")
            .Match("(t:Transaction)")
            .Where(fun t -> t.TransactionHash = transactionHash)
            .Create("t-[:pays {amount: {balance}}]->a")
            .WithParam("balance", value)
            .ExecuteWithoutResults()

    let payFromAddress transactionHash address value =
        client.Cypher
            .Merge("(a:Address { Address: {param}})")
            .WithParam("param", address)
            .OnCreate()
            .Set("a.Balance = {balance}")
            .OnMatch()
            .Set("a.Balance = a.Balance - {balance}")
            .With("a")
            .Match("(t:Transaction)")
            .Where(fun t -> t.TransactionHash = transactionHash)
            .Create("a-[:pays {amount: {balance}}]->t")
            .WithParam("balance", value)
            .ExecuteWithoutResults()

    let neoOutputOfOutput transactionHash i (output: Output): NeoOutput =
        let extractAddressFromScript canonicalScript =
            match canonicalScript with
            | PayToPublicKey address -> address.AsString
            | PayToAddress address -> address.AsString
        let address = output.CanonicalOutputScript |> Option.map extractAddressFromScript
        let address =
            match address with 
            | Some address -> 
                payToAddress transactionHash address output.Value
                address 
            | _ -> null
        { Value = output.Value
          Address = address
          Index = i }
        |> saveNeoOutput transactionHash

    let lookupTransaction transHash i =
        client.Cypher
            .Match("(t:Transaction)-[:output]->(o:Output)")
            .Where(fun t -> t.TransactionHash = transHash)
            .AndWhere(fun o -> o.Index = i)
            .Return<NeoOutput>("o")
            .Results
            .SingleOrDefault()

    let saveNeoInput transactionHash neoInput =
        client.Cypher
            .Create("(i:Input {param})")
            .WithParam("param", neoInput)
            .With("i")
            .Match("(t:Transaction)")
            .Where(fun t -> t.TransactionHash = transactionHash)
            .CreateUnique("t-[:input]->i")
            .Return<NeoInput>("i")
            .Results
            .Single()

    let neoInputOfInput transactionHash (input: Input)  =
        let inputHash = Conversion.littleEndianBytesToHexString input.InputHash
        let outputAddress, outputValue =
            if input.InputTransactionIndex = -1 then
                null, 0L
            else
                let output = lookupTransaction inputHash input.InputTransactionIndex
                if output :> obj <> null then
                    output.Address, output.Value
                else
                    printfn "can't find input %s %i" inputHash input.InputTransactionIndex
                    null, 0L
        if outputAddress <> null then
            payFromAddress transactionHash outputAddress outputValue
        { Hash =  inputHash
          Index = input.InputTransactionIndex
          Address = outputAddress
          Value = outputValue }
        |> saveNeoInput transactionHash

    let saveNeoTrans neoTrans =
        client.Cypher
            .Create("(t:Transaction {param})")
            .WithParam("param", neoTrans)
            .Return<NeoTransaction>("t")
            .Results
            .Single()

    let tansactionRelations trans blockHash =
        client.Cypher
            .Match("(t:Transaction)", "(b:Block)")
            .Where(fun t -> t.TransactionHash = trans.TransactionHash)
            .AndWhere(fun b -> b.Hash = blockHash)
            .CreateUnique("t-[:belongsTo]->b")
            .CreateUnique("t<-[:owns]-b")
            .ExecuteWithoutResults()

    let updateTransaction (trans: NeoTransaction) =
        client.Cypher
            .Match("(t:Transaction)")
            .Where(fun t -> t.TransactionHash = trans.TransactionHash)
            .Set("t.TotalInputs = {inputparam}")
            .WithParam("inputparam", trans.TotalInputs)
            .Set("t.TotalOutputs = {outputparam}")
            .WithParam("outputparam", trans.TotalOutputs)
            .ExecuteWithoutResults()

    let neoTransOfTrans (trans: Transaction) (hash: string) =
        let transactionHash = Conversion.littleEndianBytesToHexString trans.TransactionHash
        let emptyTransaction =
            { TransactionHash = transactionHash
              TotalInputs = 0L
              TotalOutputs = 0L }
            |> saveNeoTrans
        let inputs = Array.map (neoInputOfInput transactionHash)  trans.Inputs
        let outputs = Array.mapi (neoOutputOfOutput transactionHash) trans.Outputs
        let totalInputs = inputs |> Seq.sumBy (fun x -> x.Value)
        // TODO this assumes when addresss is none, the payment goes to the minor, this is usually but not always true
        let totalOutputs = outputs |> Seq.filter (fun x -> x.Address <> null) |> Seq.sumBy (fun x -> x.Value)
        let transaction = {emptyTransaction with TotalInputs = totalInputs; TotalOutputs = totalOutputs}
        updateTransaction transaction
        tansactionRelations transaction hash

    let blockRelations prev curr =
        client.Cypher
            .Match("(p:Block)", "(c:Block)")
            .Where(fun p -> p.Hash = prev.Hash)
            .AndWhere(fun c -> c.Hash = curr.Hash)
            .CreateUnique("p-[:next]->c")
            .CreateUnique("p<-[:prev]-c")
            .ExecuteWithoutResults()

    let saveNeoBlock neoBlock =
        client.Cypher
            .Create("(b:Block {param})")
            .WithParam("param", neoBlock)
            .Return<NeoBlock>("b")
            .Results
            .Single()

    let neoBlockOfBlock (block: Block) hash =
        let neoBlock = 
            { Hash = hash
              Timestamp = new DateTimeOffset(block.Timestamp, new TimeSpan(0L))  }
        let savedNeoBlock = saveNeoBlock neoBlock
        for trans in block.Transactions do
            neoTransOfTrans trans hash
        savedNeoBlock
 
    let scanBlocks (prevBlock, currBlock) (nextBlock: Block) =
        match prevBlock, currBlock with
        | Some prevNeoBlock, Some currBlock ->
            let currNeoBlock = neoBlockOfBlock currBlock (Conversion.littleEndianBytesToHexString nextBlock.Hash)
            blockRelations prevNeoBlock currNeoBlock
            Some currNeoBlock, Some nextBlock
        | None, Some currBlock ->
            let currNeoBlock = neoBlockOfBlock currBlock (Conversion.littleEndianBytesToHexString nextBlock.Hash)
            Some currNeoBlock, Some nextBlock
        | None, None -> 
            None, Some nextBlock
        | _ -> failwith "assert false"

    let load() =
        let target = "/home/robert/.bitcoin/blocks/blk00000.dat"
        let parser = BlockParserStream.FromFile(target) 
        parser.NewBlock 
        |> Event.scan scanBlocks (None, None) 
        |> ignore
        parser.StartPush()

    load()

