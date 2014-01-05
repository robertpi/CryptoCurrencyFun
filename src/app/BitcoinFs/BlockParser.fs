module BitcoinFs.BlockParser
open System
open System.Collections.Generic
open BitcoinFs.ScriptParser

type Output =
    { Value: int64
      OutputScriptLength: int64
      OutputScript: array<byte>
      ParsedOutputScript: array<Op>
      CanonicalOutputScript: option<CanonicalOutputScript> }

type Block =
    { Version: int
      Hash: array<byte>
      MerKleRoot: array<byte>
      Timestamp: DateTime
      Target: int
      Nonce: int
      Transactions: int64
      TransactionsVersion: int
      Inputs: int64
      InputHash: array<byte>
      InputTransactionIndex: int
      ResponseScriptLength: int64
      ResponseScript: array<byte>
      ParsedResponseScript: array<Op>
      SequenceNumber: int 
      NumberOfOutputs: int64
      Outputs: array<Output> }

let magicNumber = [| 0xf9uy; 0xbeuy; 0xb4uy; 0xd9uy; |]

let readOutput offSet (bytesToProcess: array<byte>) =
    let output = Conversion.bytesToInt64 bytesToProcess.[offSet .. offSet + 7]
    let challengeScriptLength, bytesUsed = Conversion.decodeVariableLengthInt bytesToProcess.[offSet + 8 .. offSet + 16]
    let offSet = offSet + 8 + bytesUsed
    let bytesUsed = offSet + int challengeScriptLength
    let script = bytesToProcess.[offSet .. bytesUsed - 1]
    let parsedScript = parseScript script |> Array.ofList
    { Value = output
      OutputScriptLength = challengeScriptLength
      OutputScript =  script
      ParsedOutputScript = parsedScript
      CanonicalOutputScript = ScriptParser.parseStandardOutputScript parsedScript }, bytesUsed

let readMessageHeader (e: IEnumerator<byte>) =
    let number = Enumerator.take 4 e
    if number.Length = 0 then -1
    else
        let gotMagicNumber = Seq.forall2 (=) magicNumber number
        if not gotMagicNumber then failwith "Error expected magicNumber, but it wasn't there"
        let bytesInBlock = Enumerator.take 4 e |> Conversion.bytesToInt32
        bytesInBlock

let readMessage bytesInBlock (e: IEnumerator<byte>) =
    let bytesToProcess = Enumerator.take bytesInBlock e
    let version = Conversion.bytesToInt32 bytesToProcess.[0 .. 3]
    let hash = bytesToProcess.[4 .. 35]
    let merkleRoot = bytesToProcess.[36 .. 67]
    let timestampInt = Conversion.bytesToInt32 bytesToProcess.[68 .. 71]
    let timestamp = Conversion.dateTimeOfUnixEpoc timestampInt
    let target = Conversion.bytesToInt32 bytesToProcess.[72 .. 75]
    let nonce = Conversion.bytesToInt32 bytesToProcess.[76 .. 79]
    let transactions, bytesUsed = Conversion.decodeVariableLengthInt bytesToProcess.[80 .. 88]
    let offSet = 80 + bytesUsed
    let transactionVersion = Conversion.bytesToInt32 bytesToProcess.[offSet .. offSet + 3]
    let inputs, bytesUsed = Conversion.decodeVariableLengthInt bytesToProcess.[offSet + 4 .. offSet + 11]
    let offSet = offSet + 4 + bytesUsed
    let inputHash = bytesToProcess.[offSet .. offSet + 31]
    let inputTransactionIndex = Conversion.bytesToInt32 bytesToProcess.[offSet + 32 .. offSet + 35]
    let responseScriptLength, bytesUsed = Conversion.decodeVariableLengthInt bytesToProcess.[offSet + 36 .. offSet + 44]
    let offSet = offSet + 36 + bytesUsed
    let responseScriptLengthInt = int responseScriptLength // assume responseScriptLength will always fit into an int32
    let responseScript = bytesToProcess.[offSet .. offSet + responseScriptLengthInt - 1] 
    let sequenceNumber = Conversion.bytesToInt32 bytesToProcess.[offSet + responseScriptLengthInt .. offSet + responseScriptLengthInt + 3]
    let numberOfOutputs, bytesUsed = Conversion.decodeVariableLengthInt bytesToProcess.[offSet + responseScriptLengthInt + 4 .. offSet + responseScriptLengthInt + 12]
    let rec loop remainingOutputs offSet acc =
        let output, offSet' = readOutput offSet bytesToProcess
        let remainingOutputs' = remainingOutputs - 1
        let acc' = output :: acc
        if remainingOutputs' > 0 then
            loop remainingOutputs' offSet' acc'
        else
            acc'
    let outputs = loop (int numberOfOutputs) (offSet + responseScriptLengthInt + 4 + bytesUsed) []
    let block =
        { Version = version
          Hash = hash
          MerKleRoot = merkleRoot
          Timestamp = timestamp
          Target = target
          Nonce = nonce
          Transactions = transactions
          TransactionsVersion = transactionVersion
          Inputs = inputs
          InputHash = inputHash
          InputTransactionIndex = inputTransactionIndex
          ResponseScriptLength = responseScriptLength
          ResponseScript = responseScript
          ParsedResponseScript = parseScript responseScript |> Array.ofList 
          SequenceNumber = sequenceNumber
          NumberOfOutputs = numberOfOutputs
          Outputs = Array.ofList outputs }

    block 

let readMessages firstMessageIndex lastMessageIndex (byteStream: seq<byte>) =
    seq { use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
          let messagesProcessed = ref 0
          while e.MoreAvailable && !messagesProcessed < lastMessageIndex do
            let bytesToProcess = readMessageHeader e
            if bytesToProcess > 0 then
                if firstMessageIndex <= !messagesProcessed then 
                    yield readMessage bytesToProcess e
                else
                    Enumerator.skip bytesToProcess e
                incr messagesProcessed }
    
let readAllMessages (byteStream: seq<byte>) =
    seq { use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
          while e.MoreAvailable do
            let bytesToProcess = readMessageHeader e
            if bytesToProcess > 0 then
                yield readMessage bytesToProcess e }
    


