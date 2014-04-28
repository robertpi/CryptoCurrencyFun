namespace global

module MultiMap =
    let empty<'T,'U when 'T : comparison> : Map<'T,'U list> = Map.empty
    let add key x multiMap =
        let prev = match Map.tryFind key multiMap with None -> [] | Some v -> v
        Map.add key (x::prev) multiMap

type Histogram<'T when 'T : comparison> = Map<'T,int>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Histogram =
    let empty<'T when 'T : comparison> : Histogram<'T> = Map.empty
    let add histogram key =
        let prev = match Map.tryFind key histogram with None -> 0 | Some v -> v
        Map.add key (prev+1) histogram

    let addMany histogram keys = Seq.fold add histogram keys

    let top n (histogram: Histogram<'T>) =
        histogram |> Seq.sortBy (fun (KeyValue(_,d)) -> -d) |> Seq.truncate n |> Seq.toArray


module Event =
    /// An event which triggers on every 'n' triggers of the input event
    let every n (ev:IEvent<_>) =
        let out = new Event<_>()
        let count = ref 0
        ev.Add (fun arg -> incr count; if !count % n = 0 then out.Trigger arg)
        out.Publish

    /// An event which triggers on every 'n' triggers of the input event
    let window n (ev:IEvent<_>) =
        let out = new Event<_>()
        let queue = System.Collections.Generic.Queue<_>()
        ev.Add (fun arg -> queue.Enqueue arg;
                           if queue.Count >= n then
                                out.Trigger (queue.ToArray());
                                queue.Dequeue() |> ignore)
        out.Publish

    let pairwise (ev:IEvent<_>) =
        let out = new Event<_>()
        let queue = System.Collections.Generic.Queue<_>()
        ev.Add (fun arg -> queue.Enqueue arg;
                           if queue.Count >= 2 then
                                let elems = queue.ToArray()
                                out.Trigger (elems.[0], elems.[1])
                                queue.Dequeue() |> ignore)
        out.Publish

    let histogram ev =
        ev |> Event.scan Histogram.addMany Histogram.empty

    let histogramBy f ev =
        ev |> Event.map f |> histogram

    let indexBy f (ev:IEvent<_>) = Event.scan (fun z x -> MultiMap.add (f x) x z) MultiMap.empty ev