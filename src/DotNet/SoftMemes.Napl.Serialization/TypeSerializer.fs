module SoftMemes.Napl.Serialization.TypeSerializer

open SoftMemes.Napl

let rec serialize t =
    let res = Serialization.NaplType()
    match t with
    | BooleanType -> res.kind <- Serialization.NaplTypeKind.BooleanTypeKind
    | StringType -> res.kind <- Serialization.NaplTypeKind.StringTypeKind
    | IntegerType -> res.kind <- Serialization.NaplTypeKind.IntegerTypeKind
    | FloatType -> res.kind <- Serialization.NaplTypeKind.FloatTypeKind
    | TupleType ts ->
        res.kind <- Serialization.NaplTypeKind.TupleTypeKind
        res.sub_types.AddRange(ts |> List.map serialize)
    | ListType t ->
        res.kind <- Serialization.NaplTypeKind.ListTypeKind
        res.sub_types.Add(serialize t)
    | SetType t ->
        res.kind <- Serialization.NaplTypeKind.SetTypeKind
        res.sub_types.Add(serialize t)
    | MapType (tk, tv) ->
        res.kind <- Serialization.NaplTypeKind.MapTypeKind
        res.sub_types.Add(serialize tk)
        res.sub_types.Add(serialize tv)
    | FunctionType (ts, t) ->
        res.kind <- Serialization.NaplTypeKind.FunctionTypeKind
        res.sub_types.Add(serialize t)
        res.sub_types.AddRange(ts |> List.map serialize)
    res

let rec deserialize (t : Serialization.NaplType) =
    match t.kind with
    | NaplTypeKind.BooleanTypeKind -> BooleanType
    | NaplTypeKind.StringTypeKind -> StringType
    | NaplTypeKind.IntegerTypeKind -> IntegerType
    | NaplTypeKind.FloatTypeKind -> FloatType
    | NaplTypeKind.TupleTypeKind ->
        t.sub_types |> Seq.map deserialize |> List.ofSeq |> TupleType
    | NaplTypeKind.ListTypeKind ->
        t.sub_types.[0] |> deserialize |> ListType
    | NaplTypeKind.SetTypeKind ->
        t.sub_types.[0] |> deserialize |> SetType
    | NaplTypeKind.MapTypeKind ->
        let tk = t.sub_types.[0] |> deserialize
        let tv = t.sub_types.[1] |> deserialize
        MapType (tk, tv)
    | NaplTypeKind.FunctionTypeKind ->
        let ts = t.sub_types |> List.ofSeq |> List.map deserialize
        FunctionType (List.tail ts, List.head ts)
    | _ -> invalidArg "naplType" "Unsupported NAPL type kind"

