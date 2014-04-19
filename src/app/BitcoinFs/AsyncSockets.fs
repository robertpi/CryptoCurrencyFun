module BitcoinFs.AsyncSockets

open System
open System.Net
open System.Net.Sockets

exception SocketIssue of SocketError with
    override this.ToString() =
        string this.Data0

/// Wraps the Socket.xxxAsync logic into F# async logic.
let inline asyncDo (op: SocketAsyncEventArgs -> bool) (prepare: SocketAsyncEventArgs -> unit)
    (select: SocketAsyncEventArgs -> 'T) =
    Async.FromContinuations <| fun (ok, error, _) ->
        let args = new SocketAsyncEventArgs()
        prepare args
        let k (args: SocketAsyncEventArgs) =
            match args.SocketError with
            | System.Net.Sockets.SocketError.Success ->
                let result = select args
                args.Dispose()
                ok result
            | e ->
                args.Dispose()
                error (SocketIssue e)
        args.add_Completed(System.EventHandler<_>(fun _ -> k))
        if not (op args) then
            k args

/// Prepares the arguments by setting the buffer.
let inline setBuffer (buf: ArraySegment<byte>) (args: SocketAsyncEventArgs) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)

let OpenSendSocket (address: IPAddress) port =
    let endPoint = new IPEndPoint(address, port)
    let socket = new Socket(address.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    asyncDo socket.ConnectAsync (fun x -> x.RemoteEndPoint <- endPoint)
        (fun a -> a.ConnectSocket)

let Accept (socket: Socket) =
    asyncDo socket.AcceptAsync ignore (fun a -> a.AcceptSocket)

let Receive (socket: Socket) (buf: ArraySegment<byte>) =
    asyncDo socket.ReceiveAsync (setBuffer buf)
        (fun a -> a.BytesTransferred)

let Send (socket: Socket) (buf: ArraySegment<byte>) =
    asyncDo socket.SendAsync (setBuffer buf) ignore