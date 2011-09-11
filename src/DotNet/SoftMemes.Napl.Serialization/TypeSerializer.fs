module SoftMemes.Napl.Serialization.TypeSerializer

open SoftMemes.Napl

let rec serialize t =
    let res = Serialization.NaplType()
    match t with
    | BooleanType -> res.kind <- Serialization.NaplTypeKind.BooleanKind
    | StringType -> res.kind <- Serialization.NaplTypeKind.StringKind
    | IntegerType -> res.kind <- Serialization.NaplTypeKind.IntegerKind
    | FloatType -> res.kind <- Serialization.NaplTypeKind.FloatKind
    | TupleType ts ->
        res.kind <- Serialization.NaplTypeKind.TupleKind
        res.sub_types.AddRange(ts |> List.map serialize)
    | ListType t ->
        res.kind <- Serialization.NaplTypeKind.ListKind
        res.sub_types.Add(serialize t)
    | SetType t ->
        res.kind <- Serialization.NaplTypeKind.SetKind
        res.sub_types.Add(serialize t)
    | MapType (tk, tv) ->
        res.kind <- Serialization.NaplTypeKind.MapKind
        res.sub_types.Add(serialize tk)
        res.sub_types.Add(serialize tv)
    | FunctionType (ts, t) ->
        res.kind <- Serialization.NaplTypeKind.FunctionKind
        res.sub_types.Add(serialize t)
        res.sub_types.AddRange(ts |> List.map serialize)
    res

let rec deserialize (t : Serialization.NaplType) =
    match t.kind with
    | NaplTypeKind.BooleanKind -> BooleanType
    | NaplTypeKind.StringKind -> StringType
    | NaplTypeKind.IntegerKind -> IntegerType
    | NaplTypeKind.FloatKind -> FloatType
    | NaplTypeKind.TupleKind ->
        t.sub_types |> Seq.map deserialize |> List.ofSeq |> TupleType
    | NaplTypeKind.ListKind ->
        t.sub_types.[0] |> deserialize |> ListType
    | NaplTypeKind.SetKind ->
        t.sub_types.[0] |> deserialize |> SetType
    | NaplTypeKind.MapKind ->
        let tk = t.sub_types.[0] |> deserialize
        let tv = t.sub_types.[1] |> deserialize
        MapType (tk, tv)
    | NaplTypeKind.FunctionKind ->
        let ts = t.sub_types |> List.ofSeq |> List.map deserialize
        FunctionType (List.tail ts, List.head ts)
    | _ -> invalidArg "naplType" "Unsupported NAPL type kind"

