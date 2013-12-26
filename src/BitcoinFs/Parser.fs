module BitcoinFs.Parser
open System
open System.Collections.Generic
open System.IO

let getByteStream file =
    seq { use stream = File.OpenRead(file)
          let read = ref 1
          let buffer: array<byte> = Array.zeroCreate 4096
          while !read > 0 do
            read := stream.Read(buffer, 0, buffer.Length)
            yield! buffer.[0 .. !read - 1] }

let take count (e: IEnumerator<'a>) =
    let buffer: array<'a> = Array.zeroCreate count

    for x in 0 .. count - 1 do
              if not (e.MoveNext()) then
                  failwith "no more data available in stream"
              buffer.[x] <- e.Current

    buffer

type EnumeratorObserver<'a> (e: IEnumerator<'a>) = 
    let mutable moreAvailable = true
    interface System.Collections.IEnumerator with 
        member x.Current = e.Current :> obj
        member x.MoveNext() = 
            moreAvailable <- e.MoveNext()
            moreAvailable
        member x.Reset() = e.Reset()
    interface IEnumerator<'a> with 
        member x.Current = e.Current
        member x.Dispose() = e.Dispose()
    member x.MoreAvailable = moreAvailable
    static member Create e = new EnumeratorObserver<'a>(e)


let bytesToInt16 (parts: seq<byte>) =
        Seq.zip parts [0;8]
        |> Seq.sumBy (fun (a,b) -> int16 a <<< b)
let bytesToInt32 (parts: seq<byte>) =
        Seq.zip parts [0;8;16;24]
        |> Seq.sumBy (fun (a,b) -> int a <<< b)
let bytesToInt64 (parts: seq<byte>) =
        Seq.zip parts [0;8;16;24]
        |> Seq.sumBy (fun (a,b) -> int64 a <<< b)

let unixEpoc = new DateTime(1970, 1, 1)
let dateTimeOfUnixEpoc (i: int) = unixEpoc.AddSeconds(float i)

let decodeVariableLengthInt (bytes: byte[]) =
    match bytes.[0] with
    | x when x < 0xfduy -> int64 x, 1
    | x when x = 0xfduy -> int64 (bytesToInt16 bytes.[1 .. 2]), 3
    | x when x = 0xfeuy -> int64 (bytesToInt32 bytes.[1 .. 4]), 5
    | x when x = 0xffuy -> int64 (bytesToInt32 bytes.[1 .. 8]), 9
    | _ -> failwith "unexpectedly large byte :)"

type Output =
    { Value: int64
      ChallengeScriptLength: int64
      ChallengeScript: array<byte> }

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
      SequenceNumber: int 
      NumberOfOutputs: int64
      Outputs: array<Output> }

let magicNumber = [| 0xf9uy; 0xbeuy; 0xb4uy; 0xd9uy; |]

let readOutput offSet (bytesToProcess: array<byte>) =
    let output = bytesToInt64 bytesToProcess.[offSet .. offSet + 7]
    let challengeScriptLength, bytesUsed = decodeVariableLengthInt bytesToProcess.[offSet + 8 .. offSet + 16]
    let offSet = offSet + 8 + bytesUsed
    let bytesUsed = offSet + int challengeScriptLength
    printfn "challengeScriptLength: %i bytesUsed: %i bytesToProcess.Length: %i" challengeScriptLength bytesUsed bytesToProcess.Length
    { Value = output
      ChallengeScriptLength = challengeScriptLength
      ChallengeScript = bytesToProcess.[offSet .. bytesUsed - 1] }, bytesUsed

let readMessage (e: IEnumerator<'a>) =
    let number = take 4 e
    let gotMagicNumber = Seq.forall2 (=) magicNumber number
    if not gotMagicNumber then failwith "Error expected magicNumber, but it wasn't there"
    let bytesInBlock = take 4 e |> bytesToInt32
    let bytesToProcess = take bytesInBlock e
    let version = bytesToInt32 bytesToProcess.[0 .. 3]
    let hash = bytesToProcess.[4 .. 35]
    let merkleRoot = bytesToProcess.[36 .. 67]
    let timestampInt = bytesToInt32 bytesToProcess.[68 .. 71]
    let timestamp = dateTimeOfUnixEpoc timestampInt
    let target = bytesToInt32 bytesToProcess.[72 .. 75]
    let nonce = bytesToInt32 bytesToProcess.[76 .. 79]
    let transactions, bytesUsed = decodeVariableLengthInt bytesToProcess.[80 .. 88]
    let offSet = 80 + bytesUsed
    let transactionVersion = bytesToInt32 bytesToProcess.[offSet .. offSet + 3]
    let inputs, bytesUsed = decodeVariableLengthInt bytesToProcess.[offSet + 4 .. offSet + 11]
    let offSet = offSet + 4 + bytesUsed
    let inputHash = bytesToProcess.[offSet .. offSet + 31]
    let inputTransactionIndex = bytesToInt32 bytesToProcess.[offSet + 32 .. offSet + 35]
    let responseScriptLength, bytesUsed = decodeVariableLengthInt bytesToProcess.[offSet + 36 .. offSet + 44]
    let offSet = offSet + 36 + bytesUsed
    let responseScriptLengthInt = int responseScriptLength // assume responseScriptLength will always fit into an int32
    let responseScript = bytesToProcess.[offSet .. offSet + responseScriptLengthInt - 1] 
    let sequenceNumber = bytesToInt32 bytesToProcess.[offSet + responseScriptLengthInt .. offSet + responseScriptLengthInt + 3]
    let numberOfOutputs, bytesUsed = decodeVariableLengthInt bytesToProcess.[offSet + responseScriptLengthInt + 4 .. offSet + responseScriptLengthInt + 12]
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
          SequenceNumber = sequenceNumber
          NumberOfOutputs = numberOfOutputs
          Outputs = Array.ofList outputs }

    printfn "%A" block 

let readMessages maxMessages (byteStream: seq<byte>) =
    use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
    let messagesProcessed = ref 0
    while e.MoreAvailable && !messagesProcessed < maxMessages do
        readMessage (e :> IEnumerator<byte>)
        incr messagesProcessed

let target = "/home/robert/.bitcoin/blocks/blk00000.dat"

let stream = getByteStream target 
readMessages 1 stream

