namespace BitcoinFs.Neo4jClient
open System
open BitcoinFs

[<CLIMutable>]
type NeoInput = 
    { Hash: string
      Index: int
      Address: string }

[<CLIMutable>]
type NeoOuput = 
    { Value: int64
      Address: option<string> }

[<CLIMutable>]
type NeoTransaction = 
    { TransactionHash: string
      Inputs: array<NeoInput>
      TotalInputs: int64
      Outputs: array<NeoOuput>
      TotalOutputs: int64 }

[<CLIMutable>]
type NeoBlock = 
    { Hash: string
      Timestamp: DateTime } 

[<CLIMutable>]
type NeoAddress = 
    { Address: string }

module LoadBlockChainModel =
    let neoOutputOfOutput (output: Output): NeoOuput =
        let extractAddressFromScript canonicalScript =
            match canonicalScript with
            | PayToPublicKey address -> address.AsString
            | PayToAddress address -> address.AsString
        let address = output.CanonicalOutputScript |> Option.map extractAddressFromScript
        { Value = output.Value
          Address = address }

    let neoInputOfInput (input: Input) =
        let address = "" // TODO need to lookup transaction and use index to find output
        { Hash =  Conversion.littleEndianBytesToHexString input.InputHash
          Index = input.InputTransactionIndex
          Address = address }

    let neoTransOfTrans (trans: Transaction) =
        let inputs = Array.map neoInputOfInput trans.Inputs
        let outputs = Array.map neoOutputOfOutput trans.Outputs
        let hash = Conversion.littleEndianBytesToHexString trans.TransactionHash
        // TODO input don't themselves hold a value, need to get that from the corrisponding output
        let totalInputs = 0L//inputs |> Seq.sumBy (fun x -> x.Value)
        // TODO this assumes when addresss is none, the payment goes to the minor, this is usually but not always true
        let totalOutputs = outputs |> Seq.filter (fun x -> Option.isSome x.Address) |> Seq.sumBy (fun x -> x.Value)
        { TransactionHash = hash
          Inputs = inputs
          TotalInputs = totalInputs
          Outputs = outputs
          TotalOutputs = totalOutputs }: NeoTransaction // TODO sum outputs

    let neoBlockOfBlock (block: Block) =
        let transactions = [||]
        { Hash = Conversion.littleEndianBytesToHexString block.Hash
          Timestamp = block.Timestamp } 

    let treatWindow window =
        match window with
        | [| prev; curr; next |] -> ()
        | _ -> failwith "unexpected window size"

    let load() =
        let stream = File.getByteStream "/home/robert/.bitcoin/blocks/blk00000.dat" 
        let blocks = BlockParser.readAllMessages (fun e message -> printfn "%O" e ) stream
        let windowedBlocks = Seq.windowed 3 blocks
        for blockWindow in windowedBlocks do
            treatWindow blockWindow

