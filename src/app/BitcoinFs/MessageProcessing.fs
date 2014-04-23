namespace BitcoinFs
open System.Net
open BitcoinFs.Messages

type CheckMessageResult =
    | Incomplete
    | Corrupt of byte[]
    | Complete of byte[] * byte[]

module MessageProcessing =
    open System.Security.Cryptography
    open BitcoinFs.CommonMessages

    let createBufferWithHeaderFromBuffer messageBuffer messageName =
        let checkSum = Crypto.ComputerCheckSum messageBuffer
        let header = 
            BitcoinFs.CommonMessages.RawMessageHeader.Create 
                Const.MagicNumber messageName (uint32 messageBuffer.Length) checkSum
        [| yield! header.Serialize(); yield! messageBuffer |]

    let createBufferWithHeader (message: IBinarySerializable<'a>) =
        let messageBuffer = message.Serialize()
        createBufferWithHeaderFromBuffer messageBuffer

    let checkMessage header totalRead segments =
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

    let processMessage header buffer replyChannel =
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
            let verack = createBufferWithHeaderFromBuffer [||] MessageNames.Verack
            replyChannel verack 
        | MessageNames.Verack ->
            printfn "verack"
        | MessageNames.Ping ->
            let ping = PingPong.Parse buffer
            let buffer = createBufferWithHeader ping MessageNames.Pong
            replyChannel buffer
        | _ -> 
            printfn "unknow message: %A" header
