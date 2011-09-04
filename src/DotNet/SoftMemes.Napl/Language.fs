namespace SoftMemes.Napl.Language

open System

type NaplType
    = BooleanType
    | StringType
    | IntegerType
    | FloatType
    | TupleType of NaplType list
    | ListType of NaplType
    | SetType of NaplType
    | MapType of NaplType * NaplType
    | FunctionType of NaplType list * NaplType

[<CustomEquality>]
[<CustomComparison>]
type Parameter = Parameter of NaplType * string
    with
        override param.Equals(other) = obj.ReferenceEquals(param, other)
        interface IComparable with
            member param.CompareTo(otherParam) = compare (param.GetHashCode()) (otherParam.GetHashCode())

type Value
    = BooleanValue of bool
    | IntegerValue of int
    | FloatValue of float
    | StringValue of string

type Operator
    // Equality
    = EqualsOperator
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
    // Collections
    | AppendOperator
    | RemoveOperator
    | UnionOperator
    | MinusOperator
    | ZipOperator
    | ContainsOperator
    | LookupOperator
    | FoldOperator
    // Control flow
    | IfOperator

type 
    [<StructuralEquality>]
    [<StructuralComparison>]
    'a TaggedNaplExpression when 'a : comparison
    = TaggedNaplExpression of ('a * 'a NaplExpression)
and 
    [<StructuralEquality>]
    [<StructuralComparison>]
    'a NaplExpression when 'a : comparison
    = ValueExpression of Value
    | TupleExpression of 'a TaggedNaplExpression list
    | ParameterExpression of Parameter
    | LetExpression of Parameter * 'a TaggedNaplExpression * 'a TaggedNaplExpression
    | MatchExpression of Parameter list * 'a TaggedNaplExpression * 'a TaggedNaplExpression
    | LambdaExpression of Parameter list * 'a TaggedNaplExpression
    | CallExpression of 'a TaggedNaplExpression * 'a TaggedNaplExpression list
    | CollectionExpression of NaplType * 'a TaggedNaplExpression list
    | OperatorExpression of Operator * 'a TaggedNaplExpression list


