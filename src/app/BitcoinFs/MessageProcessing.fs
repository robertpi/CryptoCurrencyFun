namespace BitcoinFs
open BitcoinFs.Messages

type CheckMessageResult =
    | Incomplete
    | Corrupt of byte[]
    | Complete of byte[] * byte[]

module MessageProcessing =
    open System.Security.Cryptography
    open BitcoinFs.CommonMessages

    let createBufferWithHeader (message: IBinarySerializable<'a>) messageName =
        let messageBuffer = message.Serialize()
        let checkSum = Crypto.ComputerCheckSum messageBuffer
        let header = 
            BitcoinFs.CommonMessages.RawMessageHeader.Create 
                Const.MagicNumber messageName (uint32 messageBuffer.Length) checkSum
        [| yield! header.Serialize(); yield! messageBuffer |]


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
        printfn "message received: %A" header
        match header.Command.Trim() with
        | "version" -> 
            let version = Version.Parse buffer
            printfn "version %A" version
            let verack = RawMessageHeader.Create Const.MagicNumber "verack" 0u 0x3B648D5Au
            replyChannel(verack.Serialize())
        | "verack" ->
            printfn "verack"
        | _ -> printfn "unknow message"