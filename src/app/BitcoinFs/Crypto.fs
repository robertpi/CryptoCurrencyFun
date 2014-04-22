module BitcoinFs.Crypto
open System
open System.Security.Cryptography
let Rnd = new Random()

let ComputerCheckSum (buffer: array<byte>) =
    let sha256 = SHA256.Create()
    let hash = sha256.ComputeHash(sha256.ComputeHash(buffer))
    let checkSum, offset = Conversion.bytesToUInt32 0 hash
    checkSum

let CreateNonce64() =
    // this strikes me as a poor way of doing this ...
    Rnd.NextDouble() * (float UInt64.MaxValue) |> uint64
