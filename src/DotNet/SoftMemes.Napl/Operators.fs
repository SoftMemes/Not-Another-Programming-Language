namespace SoftMemes.Napl

type NaplOperator
    // Control flow
    = ConditionOperator
    // Equality
    | EqualsOperator
    | NotEqualsOperator
    // Ordering
    | LessThanOperator
    | LessThanEqualsOperator
    | GreaterThanOperator
    | GreaterThanEqualsOperator
    // Logic
    | NotOperator
    | AndOperator
    | OrOperator
    // Numeric
    | NegateOperator
    | AddOperator
    | SubtractOperator
    | MultiplyOperator
    | DivideOperator
    // Tuples
    | CurryOperator
    | UncurryOperator
    // Collections
    | AppendOperator
    | RemoveOperator
    | UnionOperator
    | MinusOperator
    | ZipOperator
    | ContainsOperator
    | LookupOperator
    | FoldOperator

module Operators =
    type private NaplOperatorFamily
        = ControlFlowOperatorFamily
        | EqualityOperatorFamily
        | OrderingOperatorFamily
        | LogicOperatorFamily
        | NumericOperatorFamily
        | TupleOperatorFamily
        | CollectionOperatorFamily

    type private NaplOperatorArity
        = UnaryOperatorArity
        | BinaryOperatorArity
        | TrinaryOperatorArity

    let private categorizeOperator =
        function
        | ConditionOperator         -> ControlFlowOperatorFamily, TrinaryOperatorArity
        // Equality
        | EqualsOperator
        | NotEqualsOperator         -> EqualityOperatorFamily, BinaryOperatorArity
        // Ordering
        | LessThanOperator
        | LessThanEqualsOperator
        | GreaterThanOperator
        | GreaterThanEqualsOperator -> OrderingOperatorFamily, BinaryOperatorArity
        // Logic
        | NotOperator               -> LogicOperatorFamily, UnaryOperatorArity
        | AndOperator
        | OrOperator                -> LogicOperatorFamily, BinaryOperatorArity
        // Numeric
        | NegateOperator            -> NumericOperatorFamily, UnaryOperatorArity
        | AddOperator
        | SubtractOperator
        | MultiplyOperator
        | DivideOperator            -> NumericOperatorFamily, BinaryOperatorArity
        // Tuples
        | CurryOperator
        | UncurryOperator           -> TupleOperatorFamily, UnaryOperatorArity
        // Collections
        | AppendOperator
        | RemoveOperator 
        | UnionOperator
        | MinusOperator
        | ZipOperator
        | ContainsOperator
        | LookupOperator            -> CollectionOperatorFamily, BinaryOperatorArity
        | FoldOperator              -> CollectionOperatorFamily, TrinaryOperatorArity

    let (|ControlFlowOperator|EqualityOperator|OrderingOperator|LogicOperator|NumericOperator|TupleOperator|CollectionOperator|) opr =
        match categorizeOperator opr |> fst with
        | ControlFlowOperatorFamily -> ControlFlowOperator
        | EqualityOperatorFamily -> EqualityOperator
        | OrderingOperatorFamily -> OrderingOperator
        | LogicOperatorFamily -> LogicOperator
        | NumericOperatorFamily -> NumericOperator
        | TupleOperatorFamily -> TupleOperator
        | CollectionOperatorFamily -> CollectionOperator

    let (|UnaryOperator|BinaryOperator|TrinaryOperator|) opr =
        match categorizeOperator opr |> snd with
        | UnaryOperatorArity -> UnaryOperator
        | BinaryOperatorArity -> BinaryOperator
        | TrinaryOperatorArity -> TrinaryOperator

    let private setCollectionType ct t' =
        match ct, t' with
        | ListType _, _ -> ListType t'
        | SetType _, _ -> SetType t'
        | MapType _, (TupleType [tk;tv]) -> MapType (tk, tv)
        | MapType _, t -> invalidArg "t'" "Type argument must be a tuple for maps"
        | _ -> invalidArg "ct" "Expected collection type"
