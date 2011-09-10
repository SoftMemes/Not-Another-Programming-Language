namespace SoftMemes.Napl

type NaplType
    = BooleanType
    | StringType
    | IntegerType
    | FloatType
    | TupleType of NaplType list
    | FunctionType of NaplType list * NaplType
    | ListType of NaplType
    | SetType of NaplType
    | MapType of NaplType * NaplType

module Types =
    let (|NumericType|_|) =
        function
        | IntegerType
        | FloatType -> Some ()
        | _ -> None

    let (|CollectionType|_|) =
        function
        | ListType t -> Some t
        | SetType t -> Some t
        | MapType (tk, tv) -> Some (TupleType [tk;tv])
        | _ -> None
