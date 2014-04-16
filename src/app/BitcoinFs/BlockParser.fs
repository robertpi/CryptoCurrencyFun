namespace BitcoinFs
open System
open System.Diagnostics
open System.Collections.Generic
open System.Security.Cryptography
open NLog
open BitcoinFs.Messages

module BlockParser = 
    let logger = LogManager.GetLogger("BlockParser")
    let debug = false
    let debugOffset = 0 // use in cases where the message doesn't start at begin

    let magicNumber = [| 0xf9uy; 0xbeuy; 0xb4uy; 0xd9uy; |]

    let readOutput offSet (bytesToProcess: array<byte>) =
        let output, offSet = Conversion.bytesToInt64 offSet bytesToProcess
        
        logger.Debug(sprintf "output 0x%x" offSet)
        let challengeScriptLength, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess
        logger.Debug(sprintf "challengeScriptLength value 0x%x"  challengeScriptLength)

        let challengeScriptLengthInt = int challengeScriptLength
        
        logger.Debug(sprintf "challengeScript 0x%x" offSet)
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

        logger.Debug(sprintf "transactionVersion 0x%x" offSet)
        let transactionVersion, offSet = Conversion.bytesToInt32 offSet bytesToProcess

        logger.Debug(sprintf "numberOfInputs 0x%x" offSet)
        let numberOfInputs, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        let rec inputsLoop remainingInputs offSet acc =
            if remainingInputs > 0 then
                logger.Debug(sprintf "inputHash 0x%x numberOfInputs 0x%x remainingInputs 0x%x" 
                                      offSet numberOfInputs remainingInputs)
                let inputHash, offSet = bytesToProcess.[offSet .. offSet + 31], offSet + 32

                logger.Debug(sprintf "inputTransactionIndex 0x%x" offSet)
                let inputTransactionIndex, offSet = Conversion.bytesToInt32 offSet bytesToProcess

                logger.Debug(sprintf "inputTransactionIndex value 0x%x" inputTransactionIndex)

                logger.Debug(sprintf "responseScriptLength 0x%x" offSet)
                let responseScriptLength, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess
                let responseScriptLengthInt = int responseScriptLength // assume responseScriptLength will always fit into an int32
                
                logger.Debug(sprintf "responseScript 0x%x responseScriptLengthInt %x" offSet responseScriptLengthInt)
                let responseScript, offSet = Conversion.readByteBlock offSet responseScriptLengthInt  bytesToProcess
                
                logger.Debug(sprintf "sequenceNumber 0x%x" offSet)
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
                acc |> List.rev, offSet

        let inputs, offSet = inputsLoop (int numberOfInputs)  offSet []


        logger.Debug(sprintf "numberOfOutputs 0x%x" offSet)
        let numberOfOutputs, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        let rec outputLoop remainingOutputs offSet acc =
            let output, offSet' = readOutput offSet bytesToProcess
            let remainingOutputs' = remainingOutputs - 1
            let acc' = output :: acc
            if remainingOutputs' > 0 then
                outputLoop remainingOutputs' offSet' acc'
            else
                acc' |> List.rev, offSet'
        
        let outputs, offSet = outputLoop (int numberOfOutputs) offSet []
        
        logger.Debug(sprintf "lockTime 0x%x" offSet)
        let lockTime, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        let sha256 = SHA256.Create()
        let transactionHash = sha256.ComputeHash(sha256.ComputeHash(bytesToProcess, initalOffSet, offSet - initalOffSet))

        logger.Debug(sprintf "final from transaction 0x%x lockTime 0x%x" offSet lockTime)
        
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

    let readMessage initOffSet (bytesToProcess: array<byte>) =
        logger.Debug(sprintf "version 0x%x" debugOffset)
        let version, offSet = Conversion.bytesToInt32 0 bytesToProcess

        logger.Debug(sprintf "hash 0x%x" offSet)
        let hash, offSet = Conversion.readByteBlock offSet 32 bytesToProcess

        logger.Debug(sprintf "merKleRoot 0x%x" offSet)
        let merkleRoot, offSet = Conversion.readByteBlock offSet 32 bytesToProcess
        
        logger.Debug(sprintf "timestamp 0x%x" offSet)
        let timestampInt, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        let timestamp = Conversion.dateTimeOfUnixEpoc timestampInt
        
        logger.Debug(sprintf "target 0x%x" offSet)
        let target, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        logger.Debug(sprintf "nonce 0x%x" offSet)
        let nonce, offSet = Conversion.bytesToInt32 offSet bytesToProcess
        
        logger.Debug(sprintf "numberOfTransactions 0x%x" offSet)
        let numberOfTransactions, offSet = Conversion.decodeVariableLengthInt offSet bytesToProcess

        logger.Debug(sprintf "numberOfTransactions value 0x%x" numberOfTransactions)

        let rec transactionsLoop remainingTransactions offSet acc =
            let transaction, offSet' = readTransactions offSet bytesToProcess
            let remainingTransactions' = remainingTransactions - 1
            let acc' = transaction :: acc
            if remainingTransactions' > 0 then
                transactionsLoop remainingTransactions' offSet' acc'
            else
                acc' |> List.rev, offSet'

        let transactions, offSet = transactionsLoop (int numberOfTransactions) offSet []

        { OffSet = initOffSet
          Length = bytesToProcess.Length
          Version = version
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

    let readMessageHandleError offSet messageBuffer =
        try
            let message = readMessage offSet messageBuffer
            Message message
        with exc ->
            Error exc

    let readMessages initOffSet firstMessageIndex lastMessageIndex messageErrorHandler (byteStream: seq<byte>) =
        seq { use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
              let messagesProcessed = ref 0
              let offSet = ref initOffSet
              while e.MoreAvailable && !messagesProcessed < lastMessageIndex do
                  let bytesToProcess = readMessageHeader e
                  offSet := !offSet + 8L
                  if bytesToProcess > 0 then
                      if firstMessageIndex <= !messagesProcessed then
                          let messageBuffer = Enumerator.take bytesToProcess e
                          match readMessageHandleError !offSet messageBuffer with
                          | Message message -> yield message
                          | Error exc -> messageErrorHandler exc messageBuffer  
                      else
                          Enumerator.skip bytesToProcess e
                      offSet := !offSet + int64 bytesToProcess + 8L
                      incr messagesProcessed }
        
    let readAllMessages initOffSet messageErrorHandler (byteStream: seq<byte>) =
        seq { use e = EnumeratorObserver.Create(byteStream.GetEnumerator()) 
              let offSet = ref initOffSet
              while e.MoreAvailable do
                  let bytesToProcess = readMessageHeader e
                  if bytesToProcess > 0 then
                      let messageBuffer = Enumerator.take bytesToProcess e
                      match readMessageHandleError !offSet messageBuffer with
                      | Message message -> yield message
                      | Error exc -> messageErrorHandler exc messageBuffer 
                  offSet := !offSet + int64 bytesToProcess + 8L }
        

type ErrorHandler =
    | Propagate
    | Ignore
    | Custom of (Exception -> array<byte> -> unit)

exception BlockParserException of (array<byte> * Exception)

type BlockParserStream private (byteStream: seq<byte>, initOffSet) =
    let blockParsedEvent = new Event<Block>()
    let streamEventEvent = new Event<Unit>()
    let mutable errorHandler = Propagate

    let getErrorHandler() =
        match errorHandler with
        | Propagate -> fun exc message -> raise(BlockParserException(message, exc))
        | Ignore -> fun _ _ -> ()
        | Custom func -> func

    member __.NewBlock = blockParsedEvent.Publish
    member __.StreamEnded = streamEventEvent.Publish
    member __.ErrorHandler
        with get() = errorHandler
        and  set x = errorHandler <- x
    member __.StartPush() = 
        let blocks = BlockParser.readAllMessages initOffSet (getErrorHandler()) byteStream
        for block in blocks do 
            blockParsedEvent.Trigger block
        streamEventEvent.Trigger()
    member __.StartPushBetween startIndex endIndex = 
        let blocks = BlockParser.readMessages initOffSet startIndex endIndex (getErrorHandler()) byteStream
        for block in blocks do 
            blockParsedEvent.Trigger block
        streamEventEvent.Trigger()
    member __.Pull() = 
        BlockParser.readAllMessages initOffSet (getErrorHandler()) byteStream
    member __.PullBetween startIndex endIndex = 
        BlockParser.readMessages initOffSet startIndex endIndex (getErrorHandler()) byteStream

    static member FromFile (file: string, ?initOffSet) =
        let initOffSet = defaultArg initOffSet 0L
        let stream = File.getByteStream file
        new BlockParserStream(stream, initOffSet)
    static member FromDirectory (dir: string, fileSpec: string, ?initOffSet) =
        let initOffSet = defaultArg initOffSet 0L
        // TODO file spec is ignored
        let stream = BitcoinDataDir.streamFromOffSet dir initOffSet 
        new BlockParserStream(stream, initOffSet)

