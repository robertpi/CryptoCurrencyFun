namespace BitcoinFs
open System.Net
open System.Security.Cryptography
open BitcoinFs.Messages
open BitcoinFs.Constants

type CheckMessageResult =
    | Incomplete
    | Corrupt of byte[]
    | Complete of byte[] * byte[]

type internal IMessageResponseAction =
    abstract member ReplyChannel: source: IPAddress -> buffer: byte[] -> unit

type internal MessageProcessor(magicNumber, responseActions: IMessageResponseAction) =

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
                Complete(message, remaider)
            else
                Corrupt(remaider)
        else
            Incomplete

    member x.ProcessMessage header buffer address =
        match header.Command with
        | MessageNames.Version -> 
            let version = Version.Parse buffer
            let ip = IPAddress.Any
            let ipTo = 
                new IPAddress(version.AddressReceive.Address)
                |> fun x -> x.ToString()
            let ipFrom = 
                match version.Extras106 with
                | Some x -> 
                    let ip = new IPAddress(x.AddressFrom.Address)
                    ip.ToString()
                | None -> ""
            printfn "version %s -> %s %i" ipFrom ipTo version.Version
            let verack = x.CreateBufferWithHeaderFromBuffer [||] MessageNames.Verack
            responseActions.ReplyChannel address verack 
        | MessageNames.Verack ->
            printfn "verack"
        | MessageNames.Ping ->
            let ping = PingPong.Parse buffer
            let buffer = x.CreateBufferWithHeader ping MessageNames.Pong
            responseActions.ReplyChannel address buffer
        | _ -> 
            printfn "unknow message: %A" header
