namespace BitcoinFs
open System
open System.Numerics
open System.Text
open System.Security.Cryptography

module AddressHelpers =
    let bytesToHexString data =
        let builder = new StringBuilder()
        for b in data do
            builder.Append(sprintf "%02x" b) |> ignore
        builder.ToString()
    let hexStringToBytes (data: String) =
        [| for i in 0 .. 2 .. data.Length - 1 do
            yield Convert.ToByte(data.[i .. i + 1], 16) |]

    let private charTable = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

    let base58Encode (bytes: array<byte>) =
        // TODO endian question ... is only buffers from the FromPublicKey method that need reversing, or all buffers?
        let addressNum = new BigInteger(bytes |> Array.rev)
        let rec loop addressNum acc =
            if addressNum > 0I then
                let res, remainder = BigInteger.DivRem(addressNum, 58I)
                loop res (charTable.[int remainder] :: acc)
            else
                let rec appendZerosLoop i acc =
                    if bytes.[i] = 0uy then
                        appendZerosLoop (i + 1) (charTable.[0] :: acc)
                    else
                        acc
                let acc = appendZerosLoop 0 acc
                new String(Array.ofList acc)
        loop addressNum []



type Address(bytes: array<byte>) =
    static let debug = false
    
    let address = AddressHelpers.base58Encode bytes
    
    member __.AsString = address

    static member FromPublicKey(bytes: array<byte>) =
        let sha256 = SHA256.Create()
        let ripemd160 = RIPEMD160.Create()
        
        let bytes = sha256.ComputeHash bytes
        if debug then printfn "after sha256 %s" (AddressHelpers.bytesToHexString bytes)
        
        let bytes = ripemd160.ComputeHash bytes
        if debug then printfn "after ripemd160 %s" (AddressHelpers.bytesToHexString bytes)
        
        let bytes = [| yield 0uy; yield! bytes |]
        let bytesForCheckSum = sha256.ComputeHash bytes
        if debug then printfn "checkSum1 %s" (AddressHelpers.bytesToHexString bytesForCheckSum)
        
        let bytesForCheckSum = sha256.ComputeHash bytesForCheckSum
        if debug then printfn "checkSum2 %s" (AddressHelpers.bytesToHexString bytesForCheckSum)
        
        let address = [| yield! bytes; yield! bytesForCheckSum.[0 .. 3]; |]
        new Address(address)

