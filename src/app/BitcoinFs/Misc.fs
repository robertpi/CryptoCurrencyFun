namespace BitcoinFs
open System
open System.Collections.Generic
open System.IO

module File =
    let getByteStream file =
        seq { use stream = File.OpenRead(file)
              let read = ref 1
              let buffer: array<byte> = Array.zeroCreate 4096
              while !read > 0 do
                  read := stream.Read(buffer, 0, buffer.Length)
                  yield! buffer.[0 .. !read - 1] }

module Directory =
    let getByteStreamOfFiles dir spec =
        seq { let files = Directory.GetFiles(dir, spec)
              for file in files do
                  yield! File.getByteStream file }


type EnumeratorObserver<'a> (e: IEnumerator<'a>) = 
    let mutable moreAvailable = true
    interface System.Collections.IEnumerator with 
        member x.Current = e.Current :> obj
        member x.MoveNext() = 
            moreAvailable <- e.MoveNext()
            moreAvailable
        member x.Reset() = e.Reset()
    interface IEnumerator<'a> with 
        member x.Current = e.Current
        member x.Dispose() = e.Dispose()
    member x.MoreAvailable = moreAvailable
    static member Create e = new EnumeratorObserver<'a>(e) 

module Enumerator =
    let skip count (e: IEnumerator<'a>) =
        for x in 0 .. count - 1 do
                  if not (e.MoveNext()) then
                      failwith "no more data available in stream"

    let take count (e: IEnumerator<'a>) =
        let buffer: array<'a> = Array.zeroCreate count

        let mutable readBytes = 0 
        for x in 0 .. count - 1 do
            if e.MoveNext() then
                readBytes  <- readBytes + 1
                buffer.[x] <- e.Current

        if readBytes = count then buffer
        elif readBytes = 0 then Array.zeroCreate 0
        else buffer.[0 .. readBytes - 1]

module Conversion =
    let private bitShifts = Array.init 8 (fun x -> x * 8)
    let inline private genericConvert toInt startIndex length (parts: array<byte>) =
        seq { for i in 0 .. length - 1 do
                yield toInt parts.[startIndex + i] <<< bitShifts.[i] }
        |> Seq.sum, startIndex + length

    let bytesToInt16 startIndex (parts: array<byte>) =
        genericConvert int16 startIndex 2 parts

    let bytesToUInt16 startIndex (parts: array<byte>) =
        genericConvert uint16 startIndex 2 parts

    let bytesToInt32 startIndex (parts: array<byte>) =
        genericConvert int startIndex 4 parts

    let bytesToUInt32 startIndex (parts: array<byte>) =
        genericConvert uint32 startIndex 4 parts

    let bytesToInt64 startIndex (parts: array<byte>) =
        genericConvert int64 startIndex 8 parts

    let bytesToUInt64 startIndex (parts: array<byte>) =
        genericConvert uint64 startIndex 8 parts

    let unixEpoc = new DateTime(1970, 1, 1)
    let dateTimeOfUnixEpoc (i: int) = unixEpoc.AddSeconds(float i)

    let decodeVariableLengthInt startIndex (bytes: byte[]) =
        match bytes.[startIndex] with
        | x when x < 0xfduy -> int64 x, startIndex + 1
        | x when x = 0xfduy -> 
            let res, offSet =  (bytesToInt16 startIndex bytes)
            int64 res, offSet
        | x when x = 0xfeuy -> 
            let res, offSet =  (bytesToInt32 startIndex bytes)
            int64 res, offSet
        | x when x = 0xffuy ->
            let res, offSet =  (bytesToInt64 startIndex bytes)
            int64 res, offSet
        | _ -> failwith "unexpectedly large byte :)"

[<Interface>]
type IVector =
    abstract member Bytes: array<byte> 
    abstract member AsInt32: int

type Vector(b1) =
    member x.Bytes = [|b1|] 
    member x.AsInt32 =
        if b1 >= 0x80uy then
            (int (b1 &&& 0x80uy)) * -1
        else
            int b1
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32

type Vector2(b1, b2) =
    member x.Bytes = [|b1;b2;|] 
    member x.AsInt32 =
        let rawUInt16, _ = Conversion.bytesToUInt16 0 x.Bytes
        if rawUInt16 >= 0x8000us then
            (int (rawUInt16 &&& 0x8000us)) * -1
        else
            int rawUInt16
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32


type Vector4(b1, b2, b3, b4) =
    member x.Bytes = [|b1;b2;b3;b4;|] 
    member x.AsInt32 =
        let rawUInt32, _ = Conversion.bytesToUInt32 0 x.Bytes
        if rawUInt32 >= 0x80000000u then
            (int (rawUInt32 &&& 0x80000000u)) * -1
        else
            int rawUInt32
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32
