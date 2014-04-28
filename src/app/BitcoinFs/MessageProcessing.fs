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

type internal MessageProcessor(magicNumber) =

    let logger = LogManager.GetCurrentClassLogger()

    let messageReceivedEvent = new Event<_>()

    member __.MessageReceived = messageReceivedEvent.Publish

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

    member x.ProcessMessage header buffer (address: IPAddress) =
        let message =
            match header.Command |> MessageName.Parse with
            | VersionName -> 
                let version = Version.Parse buffer
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
                Version version

            | VerackName ->
                Verack
            | AddrName -> 
                let addr, _ = Address.Parse buffer 0
                Addr addr
            | InvName ->
                let invDetails, _ = InventoryDetails.Parse buffer 0
                Inv invDetails
            | GetDataName -> 
                let invDetails, _ = InventoryDetails.Parse buffer 0
                GetData invDetails
            | NotFoundName -> 
                let invDetails, _ = InventoryDetails.Parse buffer 0
                NotFound invDetails
            | GetBlocksName ->
                let getSpec, _ = GetSpec.Parse buffer 0
                GetBlocks getSpec
            | GetHeadersName -> 
                let getSpec, _ = GetSpec.Parse buffer 0
                GetHeaders getSpec
            | TxName ->
                let tx, _ = Transaction.Parse 0 buffer 
                Tx tx
            | BlockName ->
                let block, _ = Block.Parse 0L 0 buffer
                Block block
            | HeadersName ->
                let headers, _ = Headers.Parse buffer 0
                Headers headers
            | GetAddrName ->
                GetAddr
            | MemPoolName ->
                MemPool
            | CheckOrderName ->
                CheckOrder
            | SumbitOrderName ->
                SumbitOrder
            | ReplyName ->
                Reply
            | PingName ->
                let ping = PingPong.Parse buffer
                Ping ping
            | PongName ->
                let pong = PingPong.Parse buffer
                Pong pong
            | FilterLoadName -> FilterLoad
            | FilterAddName -> FilterAdd
            | FilterClearName -> FilterClear
            | MerkleBlockName -> MerkleBlock
            | AlertName ->
                let alert = Alert.Parse 0 buffer
                Alert alert
            | UnknownName message ->
                logger.Info(sprintf "unknow message: %A" header)
                Unknown (message, buffer)

        messageReceivedEvent.Trigger(address, message)
