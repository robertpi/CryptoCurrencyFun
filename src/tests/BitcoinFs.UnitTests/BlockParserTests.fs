module BitcoinFs.BlockParserTests

open NUnit.Framework
open FsUnit

[<Test>]
let shouldReadFirstThreeMessages() =
    let target = "/home/robert/.bitcoin/blocks/blk00000.dat"

    let stream = File.getByteStream target 
    let blocks = BlockParser.readMessages 0 3 stream
    printfn "%A" blocks

[<Test>]
let shouldReadMessagesFourToFive() =
    let target = "/home/robert/.bitcoin/blocks/blk00000.dat"

    let stream = File.getByteStream target 
    let blocks = BlockParser.readMessages 3 4 stream
    printfn "%A" blocks
