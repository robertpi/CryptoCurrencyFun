namespace BitcoinFs
open System.Net
open System.Security.Cryptography
open NLog
open BitcoinFs.Messages
open BitcoinFs.Constants

type CheckMessageResult =
    | Incomplete
    | Corrupt of byte[]
    | Complete of byte[] * byte[]

type internal IMessageResponseAction =
    abstract member ReplyChannel: source: IPAddress -> buffer: byte[] -> unit
    abstract member InvReceived: inv:InventoryDetails -> unit
    abstract member AddrReceived: addr:Address -> unit

type internal MessageProcessor(magicNumber, responseActions: IMessageResponseAction) =

    let logger = LogManager.GetCurrentClassLogger()

    member __.CreateBufferWithHeaderFromBuffer messageBuffer messageName =
        let checkSum = Crypto.ComputerCheckSum messageBuffer
        let header = 
            RawMessageHeader.Create 
                magicNumber messageName (uint32 messageBuffer.Length) checkSum
        [| yield! header.Serialize(); yield! messageBuffer |]

    member x.CreateBufferWithHeader (message: IBinarySerializable<'a>) =
        let messageBuffer = message.Serialize()
        x.CreateBufferWithHeaderFromBuffer messageBuffer

    member __.CheckMessage (header: RawMessageHeader) totalRead segments =
        if totalRead >= (int header.Length + RawMessageHeader.HeaderLength) then
            let completeBuffer = segments |> Seq.concat |> Seq.toArray
            let message, remaider = 
                completeBuffer.[RawMessageHeader.HeaderLength .. totalRead - 1], completeBuffer.[totalRead .. ]
            let chechSum = Crypto.ComputerCheckSum message
            if chechSum = header.Checksum then
                //logger.Debug(sprintf "Complete %A" header)
                Complete(message, remaider)
            else
                //logger.Debug(sprintf "Corrupt %A" header)
                Corrupt(remaider)
        else
            //logger.Debug(sprintf "Incomplete %A" header)
            Incomplete

    member x.ProcessMessage header buffer address =
        match header.Command |> MessageName.Parse with
        | Version -> 
            let version = Version.Parse buffer
            let ip = IPAddress.Any
            let ipTo = 
                new IPAddress(version.AddressReceive.Address)
                |> fun x -> x.ToString()
            let ipFrom, userAgent, relay, startHeight, nonce = 
                match version.Extras106 with
                | Some x -> 
                    let ip = new IPAddress(x.AddressFrom.Address)
                    ip.ToString(), x.UserAgent, x.Relay, x.StartHeight, x.Nonce
                | None -> "", "", false, 0, 0uL
            logger.Info(sprintf "version %s -> %s %i %s Sevice %i Relay %b StartHeight %i Nonce %i" 
                            ipFrom ipTo version.Version userAgent version.Service relay startHeight nonce) 
            let verack = x.CreateBufferWithHeaderFromBuffer [||] MessageNames.Verack
            responseActions.ReplyChannel address verack 
        | Verack ->
            logger.Info("verack")
        | Addr -> 
            let addr, _ = Address.Parse buffer 0
            responseActions.AddrReceived addr
        | Inv ->
            let invDetails, _ = InventoryDetails.Parse buffer 0
            responseActions.InvReceived invDetails
        | GetData -> 
            logger.Info(sprintf "%A" header)
        | NotFound -> 
            logger.Info(sprintf "%A" header)
        | GetBlocks ->
            logger.Info(sprintf "%A" header)
        | GetHeaders -> 
            logger.Info(sprintf "%A" header)
        | Tx ->
            logger.Info(sprintf "%A" header)
        | Block ->
            logger.Info(sprintf "%A" header)
        | Header ->
            logger.Info(sprintf "%A" header)
        | GetAddr ->
            logger.Info(sprintf "%A" header)
        | MemPool ->
            logger.Info(sprintf "%A" header)
        | CheckOrder ->
            logger.Info(sprintf "%A" header)
        | SumbitOrder ->
            logger.Info(sprintf "%A" header)
        | Reply ->
            logger.Info(sprintf "%A" header)
        | Ping ->
            logger.Info(sprintf "recieved ping")
            let ping = PingPong.Parse buffer
            let buffer = x.CreateBufferWithHeader ping MessageNames.Pong
            responseActions.ReplyChannel address buffer
        | Pong ->
            logger.Info(sprintf "%A" header)
        | FilterLoad ->
            logger.Info(sprintf "%A" header)
        | FilterAdd ->
            logger.Info(sprintf "%A" header)
        | FilterClear ->
            logger.Info(sprintf "%A" header)
        | MerkleBlock ->
            logger.Info(sprintf "%A" header)
        | Alert ->
            logger.Info(sprintf "%A" header)
        | Unknown message ->
            logger.Info(sprintf "unknow message: %A" header)
