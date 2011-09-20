namespace SoftMemes.Napl

module NaplTypeBuilder =
    let Boolean = BooleanType
    let String = StringType
    let Integer = IntegerType
    let Float = FloatType
    let Tuple ts = List.ofSeq ts |> TupleType
    let Function tret ts = FunctionType (List.ofSeq ts, tret)
    let List t = ListType t
    let Set t = SetType t
    let Map (tk, tv) = MapType (tk, tv)
