namespace BitcoinFs
open System
open System.Collections.Generic
open BitcoinFs.ScriptParser

type Output =
    { Value: int64
      OutputScriptLength: int64
      OutputScript: array<byte>
      ParsedOutputScript: array<Op>
      CanonicalOutputScript: option<CanonicalOutputScript> }

type Input =
    { InputHash: array<byte>
      InputTransactionIndex: int }

type Transaction = 
    { TransactionVersion: int
      NumberOfInputs: int64
      Inputs: array<Input>
      ResponseScriptLength: int64
      ResponseScript: array<byte>
      ParsedResponseScript: array<Op>
      SequenceNumber: int
      NumberOfOutputs: int64
      Outputs: array<Output>
      LockTime: int }

type Block =
    { Version: int
      Hash: array<byte>
      MerKleRoot: array<byte>
      Timestamp: DateTime
      Target: int
      Nonce: int
      NumberOfTransactions: int64
      Transactions: array<Transaction> }

module BlockParser = 
    let debug = true
    let debugOffset = 0 // use in cases where the message doesn't start at begin

    let magicNumber = [| 0xf9uy; 0xbeuy; 0xb4uy; 0xd9uy; |]

    let readByteBlock offSet length (bytesToProcess: array<byte>) =
        bytesToProcess.[offSet .. offSet + length - 1], offSet + length

    let readOutput offSet (bytesToProcess: array<byte>) =
        let output, offSet = Conversion.bytesToInt64 offSet bytesToProcess
        
        if debug then printfn "output Ox%x" (offSet + debugOffset)
        let challengeScriptLength, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess
        
        if debug then printfn "challengeScriptLength Ox%x Ox%x" (offSet + debugOffset) challengeScriptLength
        let challengeScriptLengthInt = int challengeScriptLength
        
        if debug then printfn "challengeScript Ox%x" (offSet + debugOffset)
        let challengeScript, offSet = readByteBlock offSet challengeScriptLengthInt bytesToProcess

        let parsedScript = parseScript challengeScript |> Array.ofList

        { Value = output
          OutputScriptLength = challengeScriptLength
          OutputScript =  challengeScript
          ParsedOutputScript = parsedScript
          CanonicalOutputScript = ScriptParser.parseStandardOutputScript parsedScript }, offSet

    let readTransactions offSet (bytesToProcess: array<byte>) =
        if debug then printfn "transactionVersion Ox%x" offSet
        let transactionVersion, offSet = Conversion.bytesToInt32 offSet bytesToProcess

        if debug then printfn "numberOfInputs Ox%x" (offSet + debugOffset)
        let numberOfInputs, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        let rec inputsLoop remainingInputs offSet acc =
            if remainingInputs > 0 then
                if debug then 
                    printfn "inputHash Ox%x numberOfInputs Ox%x remainingInputs Ox%x" 
                        (offSet + debugOffset) numberOfInputs remainingInputs
                let inputHash, offSet = bytesToProcess.[offSet .. offSet + 31], offSet + 32

                if debug then printfn "inputTransactionIndex Ox%x" (offSet + debugOffset)
                let inputTransactionIndex, offSet = Conversion.bytesToInt32 offSet bytesToProcess

                if debug then printfn "inputTransactionIndex value Ox%x" inputTransactionIndex

                let input =
                    { InputHash = inputHash
                      InputTransactionIndex = inputTransactionIndex }

                let remainingOutputs' = remainingInputs - 1
                let acc' = input :: acc
                inputsLoop remainingOutputs' offSet acc'
            else
                acc, offSet

        let inputs, offSet = inputsLoop (int numberOfInputs)  offSet []

        if debug then printfn "responseScriptLength Ox%x" (offSet + debugOffset)
        let responseScriptLength, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess
        let responseScriptLengthInt = int responseScriptLength // assume responseScriptLength will always fit into an int32
        
        if debug then printfn "responseScript Ox%x responseScriptLengthInt %x" (offSet + debugOffset) responseScriptLengthInt
        let responseScript, offSet = readByteBlock offSet responseScriptLengthInt  bytesToProcess
        
        if debug then printfn "sequenceNumber Ox%x" (offSet + debugOffset)
        let sequenceNumber, offSet = Conversion.bytesToInt32 offSet bytesToProcess

        if debug then printfn "numberOfOutputs Ox%x" (offSet + debugOffset)
        let numberOfOutputs, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        let rec outputLoop remainingOutputs offSet acc =
            let output, offSet' = readOutput offSet bytesToProcess
            let remainingOutputs' = remainingOutputs - 1
            let acc' = output :: acc
            if remainingOutputs' > 0 then
                outputLoop remainingOutputs' offSet' acc'
            else
                acc', offSet'
        
        let outputs, offSet = outputLoop (int numberOfOutputs) offSet []
        
        if debug then printfn "lockTime Ox%x" (offSet + debugOffset)
        let lockTime, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        if debug then printfn "final from transaction Ox%x lockTime 0x%x" (offSet + debugOffset) lockTime
        
        { TransactionVersion = transactionVersion
          NumberOfInputs = numberOfInputs
          Inputs = inputs |> Array.ofList
          ResponseScriptLength = responseScriptLength
          ResponseScript = responseScript
          ParsedResponseScript = parseScript responseScript |> Array.ofList
          SequenceNumber = sequenceNumber
          NumberOfOutputs = numberOfOutputs
          Outputs = Array.ofList outputs
          LockTime = lockTime }, offSet


    let readMessageHeader (e: IEnumerator<byte>) =
        let number = Enumerator.take 4 e
        if number.Length = 0 then -1
        else
            let gotMagicNumber = Seq.forall2 (=) magicNumber number
            if not gotMagicNumber then failwith "Error expected magicNumber, but it wasn't there"
            let bytesInBlock, _ = Enumerator.take 4 e |> Conversion.bytesToInt32 0
            bytesInBlock

    let readMessage (bytesToProcess: array<byte>) =
        if debug then printfn "version Ox%x" debugOffset
        let version, offSet = Conversion.bytesToInt32 0 bytesToProcess

        if debug then printfn "hash Ox%x" (offSet + debugOffset)
        let hash, offSet = readByteBlock offSet 32 bytesToProcess

        if debug then printfn "merKleRoot Ox%x" (offSet + debugOffset)
        let merkleRoot, offSet = readByteBlock offSet 32 bytesToProcess
        
        if debug then printfn "timestamp Ox%x" (offSet + debugOffset)
        let timestampInt, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        let timestamp = Conversion.dateTimeOfUnixEpoc timestampInt
        
        if debug then printfn "target Ox%x" (offSet + debugOffset)
        let target, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        if debug then printfn "nonce Ox%x" (offSet + debugOffset)
        let nonce, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        if debug then printfn "numberOfTransactions Ox%x" (offSet + debugOffset)
        let numberOfTransactions, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        if debug then printfn "numberOfTransactions value Ox%x" numberOfTransactions

        let rec transactionsLoop remainingTransactions offSet acc =
            let transaction, offSet' = readTransactions offSet bytesToProcess
            let remainingTransactions' = remainingTransactions - 1
            let acc' = transaction :: acc
            if remainingTransactions' > 0 then
                transactionsLoop remainingTransactions' offSet' acc'
            else
                acc', offSet'

        let transactions, offSet = transactionsLoop (int numberOfTransactions) offSet []

        { Version = version
          Hash = hash
          MerKleRoot = merkleRoot
          Timestamp = timestamp
          Target = target
          Nonce = nonce
          NumberOfTransactions = numberOfTransactions
          Transactions = Array.ofList transactions }


    type MessageResult =
        | Message of Block
        | Error of Exception

    let readMessageHandleError messageBuffer =
        try
            let message = readMessage messageBuffer
            Message message
        with exc ->
            Error exc

    let readMessages firstMessageIndex lastMessageIndex messageErrorHandler (byteStream: seq<byte>) =
        seq { use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
              let messagesProcessed = ref 0
              while e.MoreAvailable && !messagesProcessed < lastMessageIndex do
                let bytesToProcess = readMessageHeader e
                if bytesToProcess > 0 then
                    if firstMessageIndex <= !messagesProcessed then
                        let messageBuffer = Enumerator.take bytesToProcess e
                        match readMessageHandleError messageBuffer with
                        | Message message -> yield message
                        | Error exc -> messageErrorHandler exc messageBuffer  
                    else
                        Enumerator.skip bytesToProcess e
                    incr messagesProcessed }
        
    let readAllMessages messageErrorHandler (byteStream: seq<byte>) =
        seq { use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
              while e.MoreAvailable do
                let bytesToProcess = readMessageHeader e
                if bytesToProcess > 0 then
                    let messageBuffer = Enumerator.take bytesToProcess e
                    match readMessageHandleError messageBuffer with
                    | Message message -> yield message
                    | Error exc -> messageErrorHandler exc messageBuffer }
        


