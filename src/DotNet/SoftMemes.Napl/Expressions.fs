namespace SoftMemes.Napl

open System

type NaplValue
    = BooleanValue of bool
    | IntegerValue of int
    | FloatValue of float
    | StringValue of string

[<CustomEquality>]
[<CustomComparison>]
type NaplParameter = NaplParameter of NaplType * string
    with
        override param.Equals(other) = obj.ReferenceEquals(param, other)
        interface IComparable with
            member param.CompareTo(otherParam) =
                compare (param.GetHashCode()) (otherParam.GetHashCode())

[<StructuralEquality>]
[<StructuralComparison>]
type NaplExpression<'t> = NaplExpression of 't * NaplExpression'<'t>
and NaplExpression'<'t>
    = LambdaExpression of NaplParameter list * NaplExpression<'t>
    | ValueExpression of NaplValue
    | ParameterExpression of NaplParameter
    | OperatorExpression of NaplOperator * NaplExpression<'t> list
    | CollectionExpression of NaplCollectionType * NaplExpression<'t> list
    | ApplyExpression of NaplExpression<'t> * NaplExpression<'t> list
