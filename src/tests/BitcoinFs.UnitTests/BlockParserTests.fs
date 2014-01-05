module BitcoinFs.BlockParserTests

open System.Diagnostics
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

[<Test>]
let readMessagesThenParseResponseScripts() =
    let target = "/home/robert/.bitcoin/blocks/blk00000.dat"

    let stream = File.getByteStream target 
    let blocks = BlockParser.readMessages 0 100 stream
    for block in blocks do
        for output in block.Outputs do
            printfn "%A" output.CanonicalOutputScript

[<Test>]
let readAllMessagesSummarizeNonCanonical() =
    let target = "/home/robert/.bitcoin/blocks/blk00000.dat"
    let timer = Stopwatch.StartNew()
    let stream = File.getByteStream target 
    let blocks = BlockParser.readAllMessages stream
    let blockCounter = ref 0
    let payToAddress = ref 0
    let payToPK = ref 0
    for block in blocks do
        let outputCounter = ref 0
        for output in block.Outputs do
            match output.CanonicalOutputScript with
            | Some payment -> 
                match payment with
                | PayToPublicKey _ -> incr payToPK
                | PayToAddress _ -> incr payToAddress
            | None -> printfn "None canonical output block: %i output: %i" !blockCounter !outputCounter
            incr outputCounter
        incr blockCounter
    printfn "Pay To Address: %i" !payToAddress
    printfn "Pay To Public Key: %i" !payToPK
    printfn "Processed blocks: %i in %O" !blockCounter timer.Elapsed
