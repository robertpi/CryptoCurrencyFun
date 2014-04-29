module CryptoCurrencyFun.BitcoinDataDir
open System.IO

let fileFromOffSet dir offSet =
    let files = 
        Directory.GetFiles(dir, "*.dat")
        |> Seq.sortBy id
        |> Seq.map (fun x -> FileInfo(x))
        |> Seq.toList
    let rec loop offSet (files: list<FileInfo>) =
        match files with
        | file :: rest -> 
            if offSet < file.Length then offSet, file
            else loop (offSet - int64 file.Length) rest
        | [] -> failwith "not found"
    loop offSet files

let streamOfRemaining dir file offSet =
    let files = 
        Directory.GetFiles(dir, "*.dat")
        |> Seq.sortBy id
        |> Seq.skipWhile (fun x -> x <> file)
    let stream =
        seq { for file in files do
                  yield! File.getByteStream file }

    stream |> Seq.skip (int offSet)

let streamFromOffSet dir offSet =
    let offSet, file = fileFromOffSet dir offSet
    streamOfRemaining dir file.FullName offSet