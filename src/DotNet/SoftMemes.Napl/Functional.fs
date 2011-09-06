module SoftMemes.Functional

let curry f x y = f (x, y)
let uncurry f (x,y) = f x y

/// Functionally flavoured (although mutable) API for the .NET
/// Dictionary<,> type.
module HashMap =
    open System.Collections.Generic

    let add k v (dict : Dictionary<_,_>) = dict.Add(k, v)
    let remove k (dict : Dictionary<_,_>) = dict.Remove(k) |> ignore
    let find k (dict : Dictionary<_,_>) = dict.[k]
    let tryFind k (dict : Dictionary<_,_>) =
        match dict.TryGetValue(k) with
        | true, v -> Some v
        | false, _ -> None

