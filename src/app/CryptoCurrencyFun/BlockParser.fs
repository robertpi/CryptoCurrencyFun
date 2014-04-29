namespace CryptoCurrencyFun
open System
open System.Diagnostics
open System.Collections.Generic
open NLog
open CryptoCurrencyFun.Messages

module BlockParser = 
    let logger = LogManager.GetLogger("BlockParser")
    let debug = false
    let debugOffset = 0 // use in cases where the message doesn't start at begin

    let magicNumber = [| 0xf9uy; 0xbeuy; 0xb4uy; 0xd9uy; |]

    let readMessageHeader (e: IEnumerator<byte>) =
        let number = Enumerator.take 4 e
        if number.Length = 0 then -1
        else
            let gotMagicNumber = Seq.forall2 (=) magicNumber number
            if not gotMagicNumber then failwith "Error expected magicNumber, but it wasn't there"
            let bytesInBlock, _ = Enumerator.take 4 e |> Conversion.bytesToInt32 0
            bytesInBlock


    type MessageResult =
        | Message of Block
        | Error of Exception

    let readMessageHandleError initOffSet messageBuffer =
        try
            let message, _ = Block.Parse initOffSet 0 messageBuffer
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

