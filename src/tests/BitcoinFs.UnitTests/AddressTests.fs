module BitcoinFs.AddressTests

open NUnit.Framework
open FsUnit

[<Test>]
let fromWebPageExample() =
    // see https://en.bitcoin.it/wiki/Technical_background_of_Bitcoin_addresses
    let pk = "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"
    let pkBuffer = Conversion.hexStringToBytes pk
    let address = Address.FromPublicKey pkBuffer
    printfn "Address: %s 16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM" address.AsString
    // TODO assertion