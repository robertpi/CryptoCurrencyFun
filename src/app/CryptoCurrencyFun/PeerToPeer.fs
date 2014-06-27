namespace CryptoCurrencyFun
open System
open System.Collections.Generic
open System.Net
open System.Net.Sockets
open System.Security.Cryptography
open NLog
open CryptoCurrencyFun.Messages
open CryptoCurrencyFun.Constants

type internal SendConnections =
    | Add of Socket
    | Remove of Socket
    | Broadcast of byte[]
    | SendMessage of IPAddress * byte[]
    | ConnectFailed of IPAddress
    | NewAddresses of seq<IPAddress * int>

type MessageReceivedEventArgs(address: IPAddress, message: Message) =
    inherit EventArgs()
    member x.Address = address
    member x.Message = message

type internal ConnectionDetails =
    { Socket: Socket
      mutable LastPing: DateTime }
    static member Create s =
        { Socket = s
          LastPing = DateTime.MinValue }

type IPeerToPeerConnectionManager =
    abstract Connect: unit -> unit
    abstract Broadcast: Message -> unit
    abstract SendTo: IPAddress -> Message -> unit
    [<CLIEvent>]
    abstract MessageReceived: IEvent<MessageReceivedEventArgs>


type PeerToPeerConnectionManager(connDetails: NetworkDescription, ?maxConnections) =

    let logger = LogManager.GetCurrentClassLogger()

    let maxConnections = match maxConnections with Some x -> x | None -> Int32.MaxValue

    let messageProcessor =  new MessageProcessor(connDetails.MagicNumber)

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

    let startReadLoop (activeConnections: MailboxProcessor<_>) socket =

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
                        activeConnections.Post(Remove socket) }
        loop None (new ResizeArray<byte[]>())

    let createAndStoreConnection (activeConnections: MailboxProcessor<_>) address =
        async { try
                    logger.Info(sprintf "trying to connect to %O" address)
                    // TODO can we lower the timeout here? failure to connect can take a long time
                    let! conn = AsyncSockets.OpenSendSocket address connDetails.Port
                    if conn.Connected then
                        activeConnections.Post (Add conn)
                        let addressRemote, portRemote = getRemoteAddress conn
                        let addressLocal, portLocal = getLocalAddress conn
                        let version = 
                            Version.CreateMyVersion 
                                addressRemote (portRemote |> uint16) addressLocal (portLocal |> uint16)
                        let buffer = messageProcessor.CreateBufferWithHeader version MessageNames.Version
                        activeConnections.Post(SendMessage(address, buffer))
                        startReadLoop activeConnections conn |> Async.Start
                        logger.Info(sprintf "connected to %O!" address)
                    else
                        logger.Error(sprintf "didn't connect to %O" address)
                        activeConnections.Post(ConnectFailed address) 
                with ex -> 
                    logger.Error(sprintf "connection to address %O failed %O" address ex)
                    activeConnections.Post(ConnectFailed address) }



    // TODO any network connection  that throws an exception
    let makeConnectionLoop (box: MailboxProcessor<_>) =
        let addressQueue = new Queue<IPAddress>(connDetails.GetIPAddresses())
        let connections = new Dictionary<IPAddress, ConnectionDetails>()
        let pendding = new HashSet<IPAddress>()

        let connectToPeers() =
            let remainingAddress = addressQueue.Count > 0
            let currentConnections = connections.Count + pendding.Count
            let moreConnectionsNeeded = currentConnections < maxConnections
            if remainingAddress && moreConnectionsNeeded then
                let count = min addressQueue.Count (maxConnections - currentConnections) 
                logger.Debug(sprintf "count %i maxConnections %i" count maxConnections)
                let newAddresses =
                    [ for _ in 1 .. count do
                        yield addressQueue.Dequeue() ]
                for address in newAddresses do
                    pendding.Add address |> ignore
                newAddresses
                |> Seq.map (createAndStoreConnection box)
                |> Async.Parallel
                |> Async.Ignore
                |> Async.Start

        let rec connectionLoop () =
            async { try
                        connectToPeers()
                        // TODO 100ms time out is arbitary, may want to tune
                        let! msg = box.TryReceive(100)
                        let deadSockets = 
                            connections 
                            |> Seq.filter (fun x -> not x.Value.Socket.Connected)
                            |> Seq.toList
                        for deadSocket in deadSockets do
                            connections.Remove(deadSocket.Key) |> ignore
                        //logger.Debug(sprintf "Active connnections %i" connections.Count)
                        match msg with
                        | Some(Add socket) ->
                            let cd = ConnectionDetails.Create socket 
                            let ip, _ = getRemoteAddress socket
                            connections.Add(ip, cd)
                            pendding.Remove(ip) |> ignore
                            return! connectionLoop ()
                        | Some(Remove socket) -> 
                            // TODO this could fail, try removing by socket reference if it does
                            let ip, _ = getRemoteAddress socket
                            connections.Remove ip  |> ignore
                            return! connectionLoop ()
                        | Some(Broadcast buffer) ->
                            let sockets =
                                connections.Values |> Seq.map (fun x -> x.Socket) |> Seq.toArray
                            broadcast sockets buffer
                            return! connectionLoop ()
                        | Some(SendMessage (address, buffer)) ->
                            let succes, socket = 
                                connections.TryGetValue(address) 
                            match succes with
                            | true -> sendMessage socket.Socket buffer
                            | false -> logger.Error(sprintf "failed to send message to %O" address)
                            return! connectionLoop ()
                        | Some(ConnectFailed address) ->
                            pendding.Remove address |> ignore
                            return! connectionLoop ()
                        | Some(NewAddresses addresses) ->
                            for address, port in addresses do
                                if not (connections.ContainsKey address && pendding.Contains address) then
                                    // TODO ignoring port for now, will connect on default port
                                    addressQueue.Enqueue address
                            return! connectionLoop ()
                        | None ->
                            return! connectionLoop ()
                    with ex ->
                        logger.Warn(sprintf "SAVED event loop from %O" ex)
                        // TODO check if it's network excpetion and remove socket
                        return! connectionLoop () }
        connectionLoop

    let activeConnections =
        new MailboxProcessor<_>(fun box ->
                                    let connectionLoop = makeConnectionLoop box
                                    connectionLoop ())

    let sendMessageTo address messageBuffer =
        activeConnections.Post(SendMessage(address, messageBuffer))

    let startAcceptLoop() =
        // why? because msdn says so ... http://msdn.microsoft.com/en-us/library/dz10xcwh(v=vs.110).aspx
        let address = Dns.GetHostAddresses(Dns.GetHostName())
        let ipAddress = address.[0];
        let socket = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        let localEndPoint = new IPEndPoint(ipAddress, connDetails.Port);
        socket.Bind(localEndPoint)
        socket.Listen(100)
        let rec loop() =
            async { try
                        let! conn = AsyncSockets.Accept socket
                        let address = getRemoteAddress conn
                        logger.Info(sprintf "got connection from %O" address)
                        startReadLoop activeConnections conn |> Async.Start
                        return! loop()
                    with ex -> 
                        logger.Error(sprintf "readloop failed %O" ex) }
        loop()

    let handleMessage (address, message) =
        match message with
        | Version version ->
            let verack = messageProcessor.CreateBufferWithHeaderFromBuffer [||] MessageNames.Verack
            sendMessageTo address verack 
        | Verack -> ()
        | Addr address ->
            let addresses = 
                seq { for addr in address.AddressList do
                        yield (addr.GetIPAddress(), int addr.Port) }
            activeConnections.Post(NewAddresses addresses)
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

    member x.Connect() =
        startAcceptLoop() |> Async.Start
        activeConnections.Start()

    member x.Broadcast message =
        let buffer = messageProcessor.CreateBufferWithHeaderFromMessage message
        activeConnections.Post(Broadcast buffer)

    member x.SendTo address message =
        let buffer = messageProcessor.CreateBufferWithHeaderFromMessage message
        sendMessageTo address buffer

    [<CLIEvent>]
    member x.MessageReceived = 
        messageProcessor.MessageReceived
        |> Event.map toMessageRecivedEA

    interface IPeerToPeerConnectionManager with 
        member x.Connect() = x.Connect()
        member x.Broadcast message = x.Broadcast message
        member x.SendTo address message = x.SendTo address message
        [<CLIEvent>]
        member x.MessageReceived = x.MessageReceived
