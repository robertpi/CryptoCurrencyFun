namespace BitcoinFs
open System
open System.Diagnostics
open System.Collections.Generic
open System.Security.Cryptography

type Output =
    { Value: int64
      OutputScriptLength: int64
      OutputScript: array<byte>
      ParsedOutputScript: option<array<Op>>
      CanonicalOutputScript: option<CanonicalOutputScript> }

type Input =
    { InputHash: array<byte>
      InputTransactionIndex: int
      ResponseScriptLength: int64
      ResponseScript: array<byte>
      ParsedResponseScript: option<array<Op>>
      SequenceNumber: int }

type Transaction = 
    { TransactionVersion: int
      NumberOfInputs: int64
      Inputs: array<Input>
      NumberOfOutputs: int64
      Outputs: array<Output>
      LockTime: int
      TransactionHash: array<byte> }

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
    let debug = false
    let debugOffset = 0 // use in cases where the message doesn't start at begin

    let magicNumber = [| 0xf9uy; 0xbeuy; 0xb4uy; 0xd9uy; |]

    let readOutput offSet (bytesToProcess: array<byte>) =
        let output, offSet = Conversion.bytesToInt64 offSet bytesToProcess
        
        if debug then printfn "output 0x%x" (offSet + debugOffset)
        let challengeScriptLength, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess
        if debug then printfn "challengeScriptLength value 0x%x"  challengeScriptLength

        let challengeScriptLengthInt = int challengeScriptLength
        
        if debug then printfn "challengeScript 0x%x" (offSet + debugOffset)
        let challengeScript, offSet = Conversion.readByteBlock offSet challengeScriptLengthInt bytesToProcess

        let parsedScript = ScriptParser.parseScript challengeScript
        let canonicalOutputScript = Option.bind ScriptParser.parseStandardOutputScript parsedScript

        { Value = output
          OutputScriptLength = challengeScriptLength
          OutputScript =  challengeScript
          ParsedOutputScript = parsedScript
          CanonicalOutputScript =  canonicalOutputScript }, offSet

    let readTransactions offSet (bytesToProcess: array<byte>) =
        let initalOffSet = offSet

        if debug then printfn "transactionVersion 0x%x" offSet
        let transactionVersion, offSet = Conversion.bytesToInt32 offSet bytesToProcess

        if debug then printfn "numberOfInputs 0x%x" (offSet + debugOffset)
        let numberOfInputs, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        let rec inputsLoop remainingInputs offSet acc =
            if remainingInputs > 0 then
                if debug then 
                    printfn "inputHash 0x%x numberOfInputs 0x%x remainingInputs 0x%x" 
                        (offSet + debugOffset) numberOfInputs remainingInputs
                let inputHash, offSet = bytesToProcess.[offSet .. offSet + 31], offSet + 32

                if debug then printfn "inputTransactionIndex 0x%x" (offSet + debugOffset)
                let inputTransactionIndex, offSet = Conversion.bytesToInt32 offSet bytesToProcess

                if debug then printfn "inputTransactionIndex value 0x%x" inputTransactionIndex

                if debug then printfn "responseScriptLength 0x%x" (offSet + debugOffset)
                let responseScriptLength, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess
                let responseScriptLengthInt = int responseScriptLength // assume responseScriptLength will always fit into an int32
                
                if debug then printfn "responseScript 0x%x responseScriptLengthInt %x" (offSet + debugOffset) responseScriptLengthInt
                let responseScript, offSet = Conversion.readByteBlock offSet responseScriptLengthInt  bytesToProcess
                
                if debug then printfn "sequenceNumber 0x%x" (offSet + debugOffset)
                let sequenceNumber, offSet = Conversion.bytesToInt32 offSet bytesToProcess

                // nice sanity check that we've correctly found end of the responseScript
                //if sequenceNumber <> -1 then failwith "sequenceNumber number is unused so should always be -1"

                let parsedScript = ScriptParser.parseScript responseScript

                let input =
                    { InputHash = inputHash
                      InputTransactionIndex = inputTransactionIndex
                      ResponseScriptLength = responseScriptLength
                      ResponseScript = responseScript
                      ParsedResponseScript = parsedScript
                      SequenceNumber = sequenceNumber }

                let remainingOutputs' = remainingInputs - 1
                let acc' = input :: acc
                inputsLoop remainingOutputs' offSet acc'
            else
                acc, offSet

        let inputs, offSet = inputsLoop (int numberOfInputs)  offSet []


        if debug then printfn "numberOfOutputs 0x%x" (offSet + debugOffset)
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
        
        if debug then printfn "lockTime 0x%x" (offSet + debugOffset)
        let lockTime, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        let sha256 = SHA256.Create()
        let transactionHash = sha256.ComputeHash(sha256.ComputeHash(bytesToProcess, initalOffSet, offSet - initalOffSet))

        if debug then printfn "final from transaction 0x%x lockTime 0x%x" (offSet + debugOffset) lockTime
        
        { TransactionVersion = transactionVersion
          NumberOfInputs = numberOfInputs
          Inputs = inputs |> Array.ofList
          NumberOfOutputs = numberOfOutputs
          Outputs = Array.ofList outputs
          LockTime = lockTime
          TransactionHash = transactionHash }, offSet


    let readMessageHeader (e: IEnumerator<byte>) =
        let number = Enumerator.take 4 e
        if number.Length = 0 then -1
        else
            let gotMagicNumber = Seq.forall2 (=) magicNumber number
            if not gotMagicNumber then failwith "Error expected magicNumber, but it wasn't there"
            let bytesInBlock, _ = Enumerator.take 4 e |> Conversion.bytesToInt32 0
            bytesInBlock

    let readMessage (bytesToProcess: array<byte>) =
        if debug then printfn "version 0x%x" debugOffset
        let version, offSet = Conversion.bytesToInt32 0 bytesToProcess

        if debug then printfn "hash 0x%x" (offSet + debugOffset)
        let hash, offSet = Conversion.readByteBlock offSet 32 bytesToProcess

        if debug then printfn "merKleRoot 0x%x" (offSet + debugOffset)
        let merkleRoot, offSet = Conversion.readByteBlock offSet 32 bytesToProcess
        
        if debug then printfn "timestamp 0x%x" (offSet + debugOffset)
        let timestampInt, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        let timestamp = Conversion.dateTimeOfUnixEpoc timestampInt
        
        if debug then printfn "target 0x%x" (offSet + debugOffset)
        let target, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        if debug then printfn "nonce 0x%x" (offSet + debugOffset)
        let nonce, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        if debug then printfn "numberOfTransactions 0x%x" (offSet + debugOffset)
        let numberOfTransactions, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        if debug then printfn "numberOfTransactions value 0x%x" numberOfTransactions

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
        


