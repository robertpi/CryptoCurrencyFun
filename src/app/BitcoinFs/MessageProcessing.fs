namespace BitcoinFs
open BitcoinFs.Messages

type CheckMessageResult =
    | Incomplete
    | Corrupt of byte[]
    | Complete of byte[] * byte[]

module MessageProcessing =
    open System.Security.Cryptography
    open BitcoinFs.CommonMessages

    let sha256 = SHA256.Create()

    let checkMessage header totalRead segments =
        if totalRead >= (int header.Length + RawMessageHeader.HeaderLength) then
            let completeBuffer = segments |> Seq.concat |> Seq.toArray
            let message, remaider = 
                completeBuffer.[RawMessageHeader.HeaderLength .. totalRead - 1], completeBuffer.[totalRead .. ]
            let hash = sha256.ComputeHash(sha256.ComputeHash(message))
            let chechSum, offset = Conversion.bytesToUInt32 0 hash
            if chechSum = header.Checksum then
                Complete(message, remaider)
            else
                Corrupt(remaider)
        else
            Incomplete

    let processMessage header buffer replyChannel =
        printfn "message received: %A" header
        match header.Command with
        | "version" -> 
            let version = Version.Parse buffer
            printfn "version %A" version
            let verack = RawMessageHeader.Create Const.MagicNumber "verack" 0u 0x3B648D5Au
            replyChannel(verack.Serialize())
        | "verack" ->
            printfn "verack"
        | _ -> printfn "unknow message"