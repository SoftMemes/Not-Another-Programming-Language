namespace SoftMemes.Napl

open System

type NaplValue
    = BooleanValue of bool
    | IntegerValue of int
    | FloatValue of float
    | StringValue of string

[<CustomEquality>]
[<CustomComparison>]
type NaplParameter = { Type : NaplType; Name : string }
    with
        override param.Equals(other) = obj.ReferenceEquals(param, other)
        interface IComparable with
            member param.CompareTo(otherParam) =
                compare (param.GetHashCode()) (otherParam.GetHashCode())

[<StructuralEquality>]
[<StructuralComparison>]
type NaplExpression<'t> = { Annotation : 't; Expression : NaplExpression'<'t> }
and NaplExpression'<'t>
    = ValueExpression of NaplValue
    | OperatorExpression of NaplOperator * NaplExpression<'t> list
    | InstantiateExpression of NaplType * NaplExpression<'t> list
    | LambdaExpression of NaplParameter list * NaplExpression<'t>
    | ParameterExpression of NaplParameter
    | ApplyExpression of NaplExpression<'t> * NaplExpression<'t> list
