module BitcoinFs.ScriptParserTests

open NUnit.Framework
open FsUnit

let exampleResponseScripts =
    [ 
      [|4uy; 255uy; 255uy; 0uy; 29uy; 1uy; 4uy; 69uy; 84uy; 104uy; 101uy; 32uy; 84uy;
        105uy; 109uy; 101uy; 115uy; 32uy; 48uy; 51uy; 47uy; 74uy; 97uy; 110uy; 47uy;
        50uy; 48uy; 48uy; 57uy; 32uy; 67uy; 104uy; 97uy; 110uy; 99uy; 101uy; 108uy;
        108uy; 111uy; 114uy; 32uy; 111uy; 110uy; 32uy; 98uy; 114uy; 105uy; 110uy;
        107uy; 32uy; 111uy; 102uy; 32uy; 115uy; 101uy; 99uy; 111uy; 110uy; 100uy;
        32uy; 98uy; 97uy; 105uy; 108uy; 111uy; 117uy; 116uy; 32uy; 102uy; 111uy;
        114uy; 32uy; 98uy; 97uy; 110uy; 107uy; 115uy|];
    ]
let exampleOuputScripts =
    [
      [|65uy; 4uy; 103uy; 138uy; 253uy; 176uy; 254uy; 85uy; 72uy; 39uy; 25uy;
        103uy; 241uy; 166uy; 113uy; 48uy; 183uy; 16uy; 92uy; 214uy; 168uy; 40uy;
        224uy; 57uy; 9uy; 166uy; 121uy; 98uy; 224uy; 234uy; 31uy; 97uy; 222uy;
        182uy; 73uy; 246uy; 188uy; 63uy; 76uy; 239uy; 56uy; 196uy; 243uy; 85uy;
        4uy; 229uy; 30uy; 193uy; 18uy; 222uy; 92uy; 56uy; 77uy; 247uy; 186uy;
        11uy; 141uy; 87uy; 138uy; 76uy; 112uy; 43uy; 107uy; 241uy; 29uy; 95uy;
        172uy|]        
    ]

[<Test>]
let shouldParseExampleResponseScripts() =
    let doScript i script =
        printfn "Starting script %i ..." i
        let scriptResult = ScriptParser.parseScript script
        printfn "Got script: %A" scriptResult
    Seq.iteri doScript exampleResponseScripts

[<Test>]
let shouldParseExampleOutputScripts() =
    let doScript i script =
        printfn "Starting script %i ..." i
        let scriptResult = ScriptParser.parseScript script
        printfn "Got script: %A" scriptResult
        let standardScript = ScriptParser.parseStandardOutputScript (Array.ofList scriptResult)
        printfn "Got standard script: %A" standardScript
    Seq.iteri doScript exampleOuputScripts
