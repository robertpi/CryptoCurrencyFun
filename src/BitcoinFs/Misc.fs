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

        for x in 0 .. count - 1 do
                  if not (e.MoveNext()) then
                      failwith "no more data available in stream"
                  buffer.[x] <- e.Current

        buffer

module Conversion =
    let bytesToInt16 (parts: seq<byte>) =
            Seq.zip parts [0;8]
            |> Seq.sumBy (fun (a,b) -> int16 a <<< b)
    let bytesToUInt16 (parts: seq<byte>) =
            Seq.zip parts [0;8]
            |> Seq.sumBy (fun (a,b) -> uint16 a <<< b)
    let bytesToInt32 (parts: seq<byte>) =
            Seq.zip parts [0;8;16;24]
            |> Seq.sumBy (fun (a,b) -> int a <<< b)
    let bytesToUInt32 (parts: seq<byte>) =
            Seq.zip parts [0;8;16;24]
            |> Seq.sumBy (fun (a,b) -> uint32 a <<< b)
    let bytesToInt64 (parts: seq<byte>) =
            Seq.zip parts [0;8;16;24]
            |> Seq.sumBy (fun (a,b) -> int64 a <<< b)
    let bytesToUInt64 (parts: seq<byte>) =
            Seq.zip parts [0;8;16;24]
            |> Seq.sumBy (fun (a,b) -> uint64 a <<< b)

    let unixEpoc = new DateTime(1970, 1, 1)
    let dateTimeOfUnixEpoc (i: int) = unixEpoc.AddSeconds(float i)

    let decodeVariableLengthInt (bytes: byte[]) =
        match bytes.[0] with
        | x when x < 0xfduy -> int64 x, 1
        | x when x = 0xfduy -> int64 (bytesToInt16 bytes.[1 .. 2]), 3
        | x when x = 0xfeuy -> int64 (bytesToInt32 bytes.[1 .. 4]), 5
        | x when x = 0xffuy -> int64 (bytesToInt32 bytes.[1 .. 8]), 9
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
        let rawUInt16 = Conversion.bytesToUInt16 x.Bytes
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
        let rawUInt32 = Conversion.bytesToUInt32 x.Bytes
        if rawUInt32 >= 0x80000000u then
            (int (rawUInt32 &&& 0x80000000u)) * -1
        else
            int rawUInt32
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32
