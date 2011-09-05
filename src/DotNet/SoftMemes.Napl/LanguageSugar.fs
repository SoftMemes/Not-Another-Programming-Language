module SoftMemes.Napl.LanguageSugar

open SoftMemes.Napl.Language

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

let (|ControlFlowOperator|EqualityOperator|OrderingOperator|LogicOperator|NumericOperator|TupleOperator|CollectionOperator|) opr =
    match opr with
    | ConditionOperator -> ControlFlowOperator
    | EqualsOperator
    | NotEqualsOperator -> EqualityOperator
    | LessThanOperator
    | LessThanEqualsOperator
    | GreaterThanOperator
    | GreaterThanEqualsOperator -> OrderingOperator
    | NotOperator
    | AndOperator
    | OrOperator -> LogicOperator
    | NegateOperator
    | AddOperator
    | SubtractOperator
    | MultiplyOperator
    | DivideOperator -> NumericOperator
    | TupleOperator
    | CurryOperator
    | UncurryOperator -> TupleOperator
    | AppendOperator
    | RemoveOperator
    | UnionOperator
    | MinusOperator
    | ZipOperator
    | ContainsOperator
    | LookupOperator
    | FoldOperator -> CollectionOperator

let (|UnaryOperator|BinaryOperator|TrinaryOperator|) =
    function
    | NotOperator
    | NegateOperator
    | TupleOperator -> UnaryOperator
    | EqualsOperator
    | NotEqualsOperator
    | LessThanOperator
    | LessThanEqualsOperator
    | GreaterThanOperator
    | GreaterThanEqualsOperator
    | AndOperator
    | OrOperator
    | AddOperator
    | SubtractOperator
    | MultiplyOperator
    | DivideOperator
    | TupleOperator
    | CurryOperator
    | UncurryOperator
    | AppendOperator
    | RemoveOperator
    | UnionOperator
    | MinusOperator 
    | ZipOperator
    | ContainsOperator
    | LookupOperator -> BinaryOperator
    | FoldOperator
    | ConditionOperator -> TrinaryOperator
