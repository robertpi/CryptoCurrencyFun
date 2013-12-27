module BitcoinFs.BlockParserTests

open NUnit.Framework
open FsUnit

[<Test>]
let shouldReadThreeMessages() =
    let target = "/home/robert/.bitcoin/blocks/blk00000.dat"

    let stream = File.getByteStream target 
    BlockParser.readMessages 3 stream
