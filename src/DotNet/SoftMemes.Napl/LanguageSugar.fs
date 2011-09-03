module SoftMemes.Napl.LanguageSugar

open SoftMemes.Napl.Language

let (|NumericType|CollectionType|OtherType|) t =
    match t with
    | IntegerType
    | FloatType -> NumericType
    | StringType
    | BooleanType
    | TupleType _ -> OtherType
    | ListType t 
    | SetType t -> CollectionType t
    | MapType (tk, tv) -> CollectionType (TupleType [tk;tv])

let (|EqualityOperator|OrderingOperator|LogicOperator|NumericOperator|CollectionOperator|ControlFlowOperator|) opr =
    match opr with
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
    | AppendOperator
    | RemoveOperator
    | UnionOperator
    | MinusOperator
    | ZipOperator
    | ContainsOperator
    | LookupOperator
    | FoldOperator -> CollectionOperator
    | IfOperator -> ControlFlowOperator

let (|UnaryOperator|BinaryOperator|TrinaryOperator|) opr =
    match opr with
    | NotOperator
    | NegateOperator -> UnaryOperator
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
    | AppendOperator
    | RemoveOperator
    | UnionOperator
    | MinusOperator 
    | ZipOperator
    | ContainsOperator
    | LookupOperator -> BinaryOperator
    | FoldOperator
    | IfOperator -> TrinaryOperator
