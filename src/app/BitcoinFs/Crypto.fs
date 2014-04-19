module BitcoinFs.Crypto
open System.Security.Cryptography

    let ComputerCheckSum (buffer: array<byte>) =
        let sha256 = SHA256.Create()
        let hash = sha256.ComputeHash(sha256.ComputeHash(buffer))
        let checkSum, offset = Conversion.bytesToUInt32 0 hash
        checkSum
