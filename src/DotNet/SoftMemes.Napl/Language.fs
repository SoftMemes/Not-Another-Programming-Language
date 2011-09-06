namespace SoftMemes.Napl.Language

open System

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

[<CustomEquality>]
[<CustomComparison>]
type NaplParameter = NaplParameter of NaplType * string
    with
        override param.Equals(other) = obj.ReferenceEquals(param, other)
        interface IComparable with
            member param.CompareTo(otherParam) = compare (param.GetHashCode()) (otherParam.GetHashCode())

type Value
    = BooleanValue of bool
    | IntegerValue of int
    | FloatValue of float
    | StringValue of string

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
    | TupleOperator
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

[<StructuralEquality>]
[<StructuralComparison>]
type NaplExpression<'t> = NaplExpression of 't * NaplExpression'<'t>
and NaplExpression'<'t>
    = LambdaExpression of NaplParameter list * NaplExpression<'t>
    | ValueExpression of Value
    | ParameterExpression of NaplParameter
    | OperatorExpression of NaplOperator * NaplExpression<'t> list
    | CollectionExpression of NaplCollectionType * NaplExpression<'t> list
    | ApplyExpression of NaplExpression<'t> * NaplExpression<'t> list
