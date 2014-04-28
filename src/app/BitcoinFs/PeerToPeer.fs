namespace BitcoinFs
open System
open System.Net
open System.Net.Sockets
open System.Security.Cryptography
open NLog
open BitcoinFs.Messages
open BitcoinFs.Constants

type internal SendConnections =
    | Add of Socket
    | Remove of Socket
    | Broadcast of byte[]
    | SendMessage of IPAddress * byte[]

module internal PeerToPeerHelpers =
    let findInitalPeers seedDnses =
        seedDnses
        |> Seq.collect (fun dns -> Dns.GetHostAddresses(dns))

type MessageReceivedEventArgs(address: IPAddress, message: Message) =
    inherit EventArgs()
    member x.Address = address
    member x.Message = message

type PeerToPeerConnectionManager(magicNumber, port, seedIps: seq<IPAddress>) as this =

    let logger = LogManager.GetCurrentClassLogger()

    let messageProcessor =  new MessageProcessor(magicNumber)

    let toMessageRecivedEA (address, message) = new MessageReceivedEventArgs(address, message)

    let addressOfEndpoint (endPoint: EndPoint) =
        match endPoint with
        | :? IPEndPoint as ipep -> ipep.Address, ipep.Port
        | _ -> 
            logger.Debug(sprintf "not an ip endpoint %O" endPoint)
            null, 0

    let getRemoteAddress (socket: Socket) =
        addressOfEndpoint socket.RemoteEndPoint

    let getLocalAddress (socket: Socket) =
        addressOfEndpoint socket.LocalEndPoint


    let sendMessage socket buffer =
        AsyncSockets.Send socket (new ArraySegment<byte>(buffer))
        |> Async.Start

    let broadcast sockets buffer =
        let buffer' = new ArraySegment<byte>(buffer)
        for socket in sockets do
            let ip = getRemoteAddress socket
            logger.Debug(sprintf "sending message to %O" ip)
            sendMessage socket buffer

    // TODO any network connection  that throws an exception
    // TODO store meta data about the connections, including last ping, verack
    let activeSendConnections =
        MailboxProcessor.Start(fun box ->
                                    let rec connectionLoop (connections: ResizeArray<Socket>) =
                                        async { let! msg = box.Receive()
                                                match msg with
                                                | Add socket -> 
                                                    connections.Add socket
                                                    return! connectionLoop connections
                                                | Remove socket -> 
                                                    connections.Remove  |> ignore
                                                    return! connectionLoop connections
                                                | Broadcast buffer ->
                                                    broadcast (connections.ToArray()) buffer
                                                    return! connectionLoop connections
                                                | SendMessage (address, buffer) ->
                                                    let socket = 
                                                        connections 
                                                        |> Seq.tryFind (fun x -> getRemoteAddress x |> fst = address)
                                                    match socket with
                                                    | Some socket -> sendMessage socket buffer
                                                    | None -> logger.Error(sprintf "failed to send message to %O" address)
                                                    return! connectionLoop connections }
                                    connectionLoop (new ResizeArray<Socket>()))

    let sendMessageTo address messageBuffer =
        activeSendConnections.Post(SendMessage(address, messageBuffer))

    let startReadLoop socket =

        let rec processSegements header (segments: ResizeArray<byte[]>) =

            let restart buffer =
                let newSegments = new ResizeArray<byte[]>()
                newSegments.Add(buffer)
                processSegements None segments

            let totalRead = segments |> Seq.sumBy (fun x -> x.Length)
            let header =
                if Option.isNone header && totalRead > RawMessageHeader.HeaderLength then
                    let totalMessageBuffer = Seq.concat segments |> Seq.toArray
                    let header = RawMessageHeader.Parse totalMessageBuffer
                    //logger.Debug(sprintf "got header %A" header)
                    Some header
                else
                    header
            let result = 
                match header with
                | Some header -> messageProcessor.CheckMessage header totalRead segments 
                | None -> Incomplete
            match result with
            | Incomplete -> header, segments
            | Corrupt(remainder) when remainder.Length > 0 -> restart remainder
            | Corrupt(_) -> None, new ResizeArray<byte[]>()
            | Complete(message, remainder) when remainder.Length > 0 -> 
                let address = getRemoteAddress socket |> fst
                messageProcessor.ProcessMessage header.Value message address
                restart remainder
            | Complete(message, _) -> 
                let address = getRemoteAddress socket |> fst
                messageProcessor.ProcessMessage header.Value message address
                None, new ResizeArray<byte[]>()

        let rec loop header (segments: ResizeArray<byte[]>) =
            async { try
                        let buffer = new ArraySegment<byte>(Array.zeroCreate 1024)
                        let! read = AsyncSockets.Receive socket buffer
                        if read > 0 then
                            //logger.Debug(sprintf "read bytes %i" read)
                            let reallyReadBuffer = buffer.Array.[0 .. read - 1]
                            segments.Add reallyReadBuffer
                        let header, segments = processSegements header segments
                        return! loop header segments
                    with ex ->
                        logger.Error(printf "reading failed for %O exception was %O" socket.RemoteEndPoint ex)
                        activeSendConnections.Post(Remove socket) }
        loop None (new ResizeArray<byte[]>())

    let createAndStoreConnection address =
        async { try
                    logger.Info(sprintf "trying to connect to %O" address)
                    let! conn = AsyncSockets.OpenSendSocket address port
                    if conn.Connected then
                        activeSendConnections.Post (Add conn)
                        let addressRemote, portRemote = getRemoteAddress conn
                        let addressLocal, portLocal = getLocalAddress conn
                        let version = 
                            Version.CreateMyVersion 
                                addressRemote (portRemote |> uint16) addressLocal (portLocal |> uint16)
                        let buffer = messageProcessor.CreateBufferWithHeader version MessageNames.Version
                        sendMessageTo address buffer
                        startReadLoop conn |> Async.Start
                        logger.Info(sprintf "connected to %O!" address)
                    else
                        logger.Error(sprintf "didn't connect to %O" address)
                with ex -> 
                    logger.Error(sprintf "connection to address %O failed %O" address ex) }

    let startAcceptLoop() =
        // why? because msdn says so ... http://msdn.microsoft.com/en-us/library/dz10xcwh(v=vs.110).aspx
        let address = Dns.GetHostAddresses(Dns.GetHostName())
        let ipAddress = address.[0];
        let socket = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        let localEndPoint = new IPEndPoint(ipAddress, port);
        socket.Bind(localEndPoint)
        socket.Listen(100)
        let rec loop() =
            async { try
                        let! conn = AsyncSockets.Accept socket
                        let address = getRemoteAddress conn
                        logger.Info(sprintf "got connection from %O" address)
                        startReadLoop conn |> Async.Start
                        return! loop()
                    with ex -> 
                        logger.Error(sprintf "readloop failed %O" ex) }
        loop()

    let openConnections() =
        seedIps
        |> Seq.map createAndStoreConnection
        |> Async.Parallel
        |> Async.Ignore
        |> Async.Start

    let handleMessage (address, message) =
        match message with
        | Version version ->
            let verack = messageProcessor.CreateBufferWithHeaderFromBuffer [||] MessageNames.Verack
            sendMessageTo address verack 
        | Verack -> ()
        | Addr address -> ()
        | Inv invDetails -> ()
        | GetData invDetails -> ()
        | NotFound invDetails -> ()
        | GetBlocks getSpec -> ()
        | GetHeaders getSpec -> ()
        | Tx trans -> ()
        | Block block -> ()
        | Headers headers -> ()
        | GetAddr -> ()
        | MemPool -> ()
        | CheckOrder -> ()
        | SumbitOrder -> ()
        | Reply -> ()
        | Ping ping ->
            let buffer = messageProcessor.CreateBufferWithHeader ping MessageNames.Pong
            sendMessageTo address buffer
        | Pong pong -> ()
        | FilterLoad -> ()
        | FilterAdd -> ()
        | FilterClear -> ()
        | MerkleBlock -> ()
        | Alert alert -> ()
        | Unknown (name, buffer) -> ()

    do messageProcessor.MessageReceived |> Event.add handleMessage

    new (magicNumber, port, seedDnses) = PeerToPeerConnectionManager(magicNumber, port, PeerToPeerHelpers.findInitalPeers seedDnses)

    // TOOD need a way to specify whether we should limit connections or try to maximize them
    member x.Connect() =
        startAcceptLoop() |> Async.Start
        openConnections()

    member x.BroadcastMemPool() =
        let buffer = messageProcessor.CreateBufferWithHeaderFromBuffer [||] MessageNames.MemPool
        activeSendConnections.Post(Broadcast buffer)

    member x.BroadcastGetAddr() =
        let buffer = messageProcessor.CreateBufferWithHeaderFromBuffer [||] MessageNames.GetAddr
        activeSendConnections.Post(Broadcast buffer)

    member x.BroadcastPing() =
        let ping = PingPong.Create()
        let buffer = messageProcessor.CreateBufferWithHeader ping MessageNames.GetAddr
        activeSendConnections.Post(Broadcast buffer)

    // TODO generic boardcast, send to specify node, send to random node methods

    [<CLIEvent>]
    member this.MessageReceived = 
        messageProcessor.MessageReceived
        |> Event.map toMessageRecivedEA
