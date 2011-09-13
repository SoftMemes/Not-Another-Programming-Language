module internal SoftMemes.Napl.Compilation.TypeCompiler

open System
open SoftMemes.Napl

let getType {NaplExpression.Annotation = (t,_)} = t

let createTupleType =
    let openTs =
        [|
            typeof<Tuple<_>>;
            typeof<Tuple<_,_>>;
            typeof<Tuple<_,_,_>>;
            typeof<Tuple<_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_,_,_,_>>;
        |] |> Array.map (fun t -> t.GetGenericTypeDefinition())
    fun ts ->
        // TODO: Bounds check
        let openT = openTs.[List.length ts - 1]
        openT.MakeGenericType(List.toArray ts)

let createFuncType =
    let openTs =
        [|
            typeof<Func<_>>;
            typeof<Func<_,_>>;
            typeof<Func<_,_,_>>;
            typeof<Func<_,_,_,_>>;
            typeof<Func<_,_,_,_,_>>;
            typeof<Func<_,_,_,_,_,_>>;
            typeof<Func<_,_,_,_,_,_,_>>;
            typeof<Func<_,_,_,_,_,_,_,_>>;
        |] |> Array.map (fun t -> t.GetGenericTypeDefinition())
    fun ts tret ->
        // TODO: Bounds check
        let openT = openTs.[List.length ts]
        openT.MakeGenericType(List.toArray (List.append ts [tret]))

let rec getNativeType = function
    | BooleanType -> typeof<bool>
    | StringType -> typeof<string>
    | IntegerType -> typeof<int32>
    | FloatType -> typeof<float>
    | TupleType ts ->
        let nativeTs = List.map getNativeType ts
        createTupleType nativeTs
    | ListType t ->
        let nativeT = getNativeType t
        typeof<List<_>>.MakeGenericType(nativeT)
    | SetType t ->
        let nativeT = getNativeType t
        typeof<Set<_>>.MakeGenericType(nativeT)
    | MapType (tkey, tvalue) ->
        let nativeTKey = getNativeType tkey
        let nativeTValue = getNativeType tvalue
        typeof<Map<_,_>>.MakeGenericType(nativeTKey, nativeTValue)
    | FunctionType (tins, tout) ->
        let nativeTins = tins |> List.map getNativeType
        let nativeTout = getNativeType tout
        createFuncType nativeTins nativeTout

