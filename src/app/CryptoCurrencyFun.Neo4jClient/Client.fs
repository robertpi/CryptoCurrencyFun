namespace CryptoCurrencyFun.Neo4jEtl
open System
open System.Linq
open System.Diagnostics
open System.Collections.Generic
open CryptoCurrencyFun
open CryptoCurrencyFun.Messages
open Neo4jClient
open Neo4jClient.Cypher
open UnionArgParser
open NLog

[<CLIMutable>]
type NeoInput = 
    { Hash: string
      Index: int
      Value: int64
      Script: byte[]
      Address: string }

[<CLIMutable;>]
type NeoOutput = 
    { Value: int64
      Script: byte[]
      Address: string
      Index: int }

[<CLIMutable>]
type NeoTransaction = 
    { TransactionHash: string
      IsRewardBlock: bool
      TotalInputs: int64
      TotalOutputs: int64 }

[<CLIMutable>]
type NeoBlock = 
    { Hash: string
      Timestamp: DateTimeOffset
      Height: int64
      OffSet: int64
      Length: int
      Target: int
      Nonce: int
      NumberOfTransactions: uint64 } 

module LoadBlockChainModel =
    let logger = LogManager.GetLogger("LoadBlockChainModel")
    type PayDirection = To | From
    type Source = Dir of string | File of string
    type Mode = 
        | Between of int * int 
        | Limit of int 
        | All

    let time name func =
        let sw = Stopwatch.StartNew() 
        let res = func()
        logger.Debug(sprintf "%s;%i" name sw.ElapsedMilliseconds)
        res

    // TODO is it worth moving any function that touches client into a seperate file?
    let doLoad url source messageScope =
        let client = new GraphClient(new Uri(url))
        client.Connect()

        let saveInputOutputRecord label transactionHash record =
            time "saveInputOutputRecord" (fun () ->
                client.Cypher
                    .Create(sprintf "(o:%s {param})" label)
                    .WithParam("param", record)
                    .With("o")
                    .Match("(t:Transaction)")
                    .Where(fun t -> t.TransactionHash = transactionHash)
                    // TODO fix direction here??
                    .CreateUnique(sprintf "t-[:%s]->o" (label.ToLowerInvariant()))
                    .ExecuteWithoutResults())

        let payAddress direction transactionHash address value transTime =
            let sign, source, dest = 
                match direction with 
                | To -> "+", "t", "a"
                | From -> "-", "a", "t"
            time "payAddress" (fun () ->
                client.Cypher
                    .Merge("(a:Address { Address: {param}})")
                    .WithParam("param", address)
                    .OnCreate()
                    .Set("a.Balance = {balance}")
                    .OnMatch()
                    .Set(sprintf "a.Balance = a.Balance %s {balance}" sign)
                    .With("a")
                    .Match("(t:Transaction)")
                    .Where(fun t -> t.TransactionHash = transactionHash)
                    .Create(sprintf "%s-[:pays {Amount: {balance}, Time: {time}}]->%s" source dest)
                    .WithParam("balance", value)
                    .WithParam("time", transTime)
                    .ExecuteWithoutResults())

        let lookupTransaction transHash i =
            time "lookupTransaction" (fun () ->
                client.Cypher
                    .Match("(t:Transaction)-[:output]->(o:Output)")
                    .Where(fun t -> t.TransactionHash = transHash)
                    .AndWhere(fun o -> o.Index = i)
                    .Return<NeoOutput>("o")
                    .Results
                    .SingleOrDefault())


        let saveRecord label record =
            time "saveRecord" (fun () ->
                client.Cypher
                    .Create(sprintf "(b:%s {param})" label)
                    .WithParam("param", record)
                    .ExecuteWithoutResults())

        let tansactionRelations trans blockHash =
            time "tansactionRelations" (fun () ->
                client.Cypher
                    .Match("(t:Transaction)", "(b:Block)")
                    .Where(fun t -> t.TransactionHash = trans.TransactionHash)
                    .AndWhere(fun b -> b.Hash = blockHash)
                    .CreateUnique("t-[:belongsTo]->b")
                    .CreateUnique("t<-[:owns]-b")
                    .ExecuteWithoutResults())

        let updateTransaction (trans: NeoTransaction) =
            time "updateTransaction" (fun () ->
                client.Cypher
                    .Match("(t:Transaction)")
                    .Where(fun t -> t.TransactionHash = trans.TransactionHash)
                    .Set("t.TotalInputs = {inputparam}")
                    .WithParam("inputparam", trans.TotalInputs)
                    .Set("t.TotalOutputs = {outputparam}")
                    .WithParam("outputparam", trans.TotalOutputs)
                    .Set("t.IsRewardBlock = {rewardparam}")
                    .WithParam("rewardparam", trans.IsRewardBlock)
                    .ExecuteWithoutResults())

        let getLastBlock() =
            time "getLastBlock" (fun () ->
                client.Cypher
                    .Match("(b:Block)")
                    .With("b")
                    .OrderByDescending("b.Height")
                    .Return<NeoBlock>("b")
                    .Limit(new Nullable<int>(1))
                    .Results
                    .SingleOrDefault())

        let neoOutputOfOutput transactionHash transTime i (output: Output): NeoOutput =
            let extractAddressFromScript canonicalScript =
                match canonicalScript with
                | PayToPublicKey address -> address.AsString
                | PayToAddress address -> address.AsString
            let address = output.CanonicalOutputScript |> Option.map extractAddressFromScript
            let address =
                match address with 
                | Some address -> 
                    payAddress To transactionHash address output.Value transTime
                    address 
                | _ -> null
            let output =
                { Value = output.Value
                  Script = output.OutputScript
                  Address = address
                  Index = i }
            saveInputOutputRecord "Output" transactionHash output
            output

        let neoInputOfInput transactionHash transTime (input: Input) =
            let inputHash = Conversion.littleEndianBytesToHexString input.InputHash
            let outputAddress, outputValue =
                if input.InputTransactionIndex = -1 then
                    null, 0L
                else
                    let output = lookupTransaction inputHash input.InputTransactionIndex
                    if output :> obj <> null then
                        output.Address, output.Value
                    else
                        logger.Warn(sprintf "can't find input %s %i" inputHash input.InputTransactionIndex)
                        null, 0L
            if outputAddress <> null then
                payAddress From transactionHash outputAddress outputValue transTime
            let input = 
                { Hash =  inputHash
                  Index = input.InputTransactionIndex
                  Script = input.ResponseScript
                  Address = outputAddress
                  Value = outputValue }
            saveInputOutputRecord "Input" transactionHash input
            input

        let neoTransOfTrans (trans: Transaction) (hash: string) timestamp =
            let transactionHash = Conversion.littleEndianBytesToHexString trans.TransactionHash
            let emptyTransaction =
                { TransactionHash = transactionHash
                  IsRewardBlock = false
                  TotalInputs = 0L
                  TotalOutputs = 0L }
            saveRecord "Transaction" emptyTransaction
            let inputs = Array.map (neoInputOfInput transactionHash timestamp)  trans.Inputs
            let isRewardBlock = inputs |> Array.exists (fun x -> x.Index = -1)
            let outputs = Array.mapi (neoOutputOfOutput transactionHash timestamp) trans.Outputs
            let totalInputs = inputs |> Seq.sumBy (fun x -> x.Value)
            // TODO this assumes when addresss is none, the payment goes to the minor, this is usually but not always true
            let totalOutputs = outputs |> Seq.filter (fun x -> x.Address <> null) |> Seq.sumBy (fun x -> x.Value)
            let transaction = {emptyTransaction with TotalInputs = totalInputs; TotalOutputs = totalOutputs; IsRewardBlock = isRewardBlock}
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


        let neoBlockOfBlock (block: Block) hash height =
            let timestamp = new DateTimeOffset(block.Timestamp, new TimeSpan(0L))
            let neoBlock = 
                { Hash = hash
                  Timestamp = timestamp
                  Height = height
                  OffSet = block.OffSet
                  Length = block.Length
                  Target = block.Target 
                  Nonce = block.Nonce
                  NumberOfTransactions = block.NumberOfTransactions }
            saveRecord "Block" neoBlock
            for trans in block.Transactions do
                neoTransOfTrans trans hash timestamp
            neoBlock
     
        let scanBlocks (prevBlock, currBlock, i) (nextBlock: Block) =
            match prevBlock, currBlock with
            | Some prevNeoBlock, Some currBlock ->
                let currNeoBlock = neoBlockOfBlock currBlock (Conversion.littleEndianBytesToHexString nextBlock.Hash) i
                blockRelations prevNeoBlock currNeoBlock
                Some currNeoBlock, Some nextBlock, (i + 1L)
            | None, Some currBlock ->
                let currNeoBlock = neoBlockOfBlock currBlock (Conversion.littleEndianBytesToHexString nextBlock.Hash) i
                Some currNeoBlock, Some nextBlock, (i + 1L)
            | None, None -> 
                None, Some nextBlock, i
            | _ -> failwith "assert false"

        let addIndexes() =
            let rawClient = client :> IRawGraphClient
            let query text = new CypherQuery(text, new Dictionary<string, obj>(), CypherResultMode.Set)
            rawClient.ExecuteCypher(query "CREATE INDEX ON :Block(Hash)")
            rawClient.ExecuteCypher(query "CREATE INDEX ON :Block(Height)")
            rawClient.ExecuteCypher(query "CREATE INDEX ON :Transaction(TransactionHash)")

        let lastBlock = getLastBlock()

        let height, offSet = 
            if lastBlock :> obj = null then 
                addIndexes()
                0L, 0L 
            else lastBlock.Height, lastBlock.OffSet

        let parser =
            match source with
            | Dir target -> 
                BlockParserStream.FromDirectory (target, "*.dat", initOffSet = offSet)
            | File target -> BlockParserStream.FromFile target
        
        let sw = Stopwatch.StartNew() 
        
        let messages =
            match messageScope with
            | Between(startMessage, endMessage) ->
                parser.PullBetween startMessage endMessage
            | Limit limit -> parser.Pull() |> Seq.take limit
            | All -> parser.Pull()

        Seq.fold scanBlocks (None, None, height) messages |> ignore

        logger.Info(sprintf "Done in %O" sw.Elapsed)


    [<EntryPoint>]
    let main args =
        // build the argument parser
        let parser = UnionArgParser<Arguments>()
        let results = parser.Parse(args)
        let dbUrl = results.GetResult (<@ Database_url @>, defaultValue = "http://localhost:7474/db/data")
        let target = 
            if results.Contains <@ Bitcoin_file @> then
                File (results.GetResult (<@ Bitcoin_file @>))
            else 
                Dir (results.GetResult ((<@ Bitcoin_dir @>), defaultValue = "/home/robert/.bitcoin/blocks"))

        let messageScope =
            if results.Contains <@ Message_between @> then
                Between (results.GetResult (<@ Message_between @>))
            elif results.Contains <@ Limit_to @> then
                Limit (results.GetResult (<@ Limit_to @>))
            else
                All

        LoggingConfig.configureLogs true true LogLevel.Info ["LoadBlockChainModel"]

        doLoad dbUrl target messageScope

        0

