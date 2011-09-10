namespace SoftMemes.Napl

type NaplType
    = BooleanType
    | StringType
    | IntegerType
    | FloatType
    | TupleType of NaplType list
    | FunctionType of NaplType list * NaplType
    | CollectionType of NaplCollectionType
and NaplCollectionType
    = ListType of NaplType
    | SetType of NaplType
    | MapType of NaplType * NaplType

module Types =
    let (|NumericType|_|) =
        function
        | IntegerType
        | FloatType -> Some ()
        | _ -> None

    let (|CollectionOfType|_|) =
        function
        | CollectionType (ListType t) -> Some t
        | CollectionType (SetType t) -> Some t
        | CollectionType (MapType (tk, tv)) -> Some (TupleType [tk;tv])
        | _ -> None

    let (|ListOfType|_|) =
        function
        | CollectionType (ListType t) -> Some t
        | _ -> None

    let (|SetOfType|_|) =
        function
        | CollectionType (SetType t) -> Some t
        | _ -> None

    let (|MapOfType|_|) =
        function
        | CollectionType (MapType (tk, tv)) -> Some (tk, tv)
        | _ -> None

    let (|MapOfKeyType|_|) =
        function
        | CollectionType (MapType (tk, _)) -> Some tk
        | _ -> None

    let (|CollectionTypeOf|) =
        function
        | ListType t
        | SetType t -> t
        | MapType (tk, tv) -> TupleType [tk;tv]
