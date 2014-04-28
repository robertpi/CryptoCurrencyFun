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
    abstract member HandleInvReceived: inv:InventoryDetails -> unit
    abstract member HandleAddrReceived: addr:Address -> unit

type internal MessageProcessor(magicNumber, responseActions: IMessageResponseAction) =

    let logger = LogManager.GetCurrentClassLogger()

    member __.CreateBufferWithHeaderFromBuffer messageBuffer messageName =
        let checkSum = Crypto.ComputerCheckSum messageBuffer
        let header = 
            RawMessageHeader.Create 
                magicNumber messageName (uint32 messageBuffer.Length) checkSum
        [| yield! header.Serialize(); yield! messageBuffer |]

    member x.CreateBufferWithHeader (message: IBinarySerializable) =
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
        | VersionName -> 
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
        | VerackName ->
            // TODO store verack wiht connection details
            logger.Info("verack")
        | AddrName -> 
            let addr, _ = Address.Parse buffer 0
            responseActions.HandleAddrReceived addr
        | InvName ->
            let invDetails, _ = InventoryDetails.Parse buffer 0
            responseActions.HandleInvReceived invDetails
        | GetDataName -> 
            logger.Info(sprintf "%A" header)
        | NotFoundName -> 
            logger.Info(sprintf "%A" header)
        | GetBlocksName ->
            logger.Info(sprintf "%A" header)
        | GetHeadersName -> 
            logger.Info(sprintf "%A" header)
        | TxName ->
            logger.Info(sprintf "%A" header)
        | BlockName ->
            logger.Info(sprintf "%A" header)
        | HeaderName ->
            logger.Info(sprintf "%A" header)
        | GetAddrName ->
            logger.Info(sprintf "%A" header)
        | MemPoolName ->
            logger.Info(sprintf "%A" header)
        | CheckOrderName ->
            logger.Info(sprintf "%A" header)
        | SumbitOrderName ->
            logger.Info(sprintf "%A" header)
        | ReplyName ->
            logger.Info(sprintf "%A" header)
        | PingName ->
            logger.Info(sprintf "recieved ping")
            let ping = PingPong.Parse buffer
            let buffer = x.CreateBufferWithHeader ping MessageNames.Pong
            responseActions.ReplyChannel address buffer
        | PongName ->
            logger.Info(sprintf "%A" header)
        | FilterLoadName ->
            logger.Info(sprintf "%A" header)
        | FilterAddName ->
            logger.Info(sprintf "%A" header)
        | FilterClearName ->
            logger.Info(sprintf "%A" header)
        | MerkleBlockName ->
            logger.Info(sprintf "%A" header)
        | AlertName ->
            logger.Info(sprintf "%A" header)
        | UnknownName message ->
            logger.Info(sprintf "unknow message: %A" header)
