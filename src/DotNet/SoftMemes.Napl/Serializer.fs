module  SoftMemes.Napl.Serializer

open SoftMemes.Napl
open SoftMemes.Napl.Language

let serializerOperator =
    function
    | EqualsOperator -> Serialization.Operator.EqualsOperator
    | NotEqualsOperator -> Serialization.Operator.NotEqualsOperator
    | LessThanOperator -> Serialization.Operator.LessThanOperator
    | LessThanEqualsOperator -> Serialization.Operator.LessThanEqualsOperator
    | GreaterThanOperator -> Serialization.Operator.GreaterThanOperator
    | GreaterThanEqualsOperator -> Serialization.Operator.GreaterThanEqualsOperator
    | NotOperator -> Serialization.Operator.NotOperator
    | AndOperator -> Serialization.Operator.AndOperator
    | OrOperator -> Serialization.Operator.OrOperator
    | NegateOperator -> Serialization.Operator.NegateOperator
    | AddOperator -> Serialization.Operator.AddOperator
    | SubtractOperator -> Serialization.Operator.SubtractOperator
    | MultiplyOperator -> Serialization.Operator.MultiplyOperator
    | DivideOperator -> Serialization.Operator.DivideOperator
    | AppendOperator -> Serialization.Operator.AppendOperator
    | RemoveOperator -> Serialization.Operator.RemoveOperator
    | UnionOperator -> Serialization.Operator.UnionOperator
    | MinusOperator -> Serialization.Operator.MinusOperator
    | ZipOperator -> Serialization.Operator.ZipOperator
    | ContainsOperator -> Serialization.Operator.ContainsOperator
    | LookupOperator -> Serialization.Operator.LookupOperator
    | FoldOperator -> Serialization.Operator.FoldOperator
    | IfOperator -> Serialization.Operator.IfOperator

let deserializerOperator =
    function
    | Serialization.Operator.EqualsOperator -> EqualsOperator
    | Serialization.Operator.NotEqualsOperator -> NotEqualsOperator
    | Serialization.Operator.LessThanOperator -> LessThanOperator
    | Serialization.Operator.LessThanEqualsOperator -> LessThanEqualsOperator
    | Serialization.Operator.GreaterThanOperator -> GreaterThanOperator
    | Serialization.Operator.GreaterThanEqualsOperator -> GreaterThanEqualsOperator
    | Serialization.Operator.NotOperator -> NotOperator
    | Serialization.Operator.AndOperator -> AndOperator
    | Serialization.Operator.OrOperator -> OrOperator
    | Serialization.Operator.NegateOperator -> NegateOperator
    | Serialization.Operator.AddOperator -> AddOperator
    | Serialization.Operator.SubtractOperator -> SubtractOperator
    | Serialization.Operator.MultiplyOperator -> MultiplyOperator
    | Serialization.Operator.DivideOperator -> DivideOperator
    | Serialization.Operator.AppendOperator -> AppendOperator
    | Serialization.Operator.RemoveOperator -> RemoveOperator
    | Serialization.Operator.UnionOperator -> UnionOperator
    | Serialization.Operator.MinusOperator -> MinusOperator
    | Serialization.Operator.ZipOperator -> ZipOperator
    | Serialization.Operator.ContainsOperator -> ContainsOperator
    | Serialization.Operator.LookupOperator -> LookupOperator
    | Serialization.Operator.FoldOperator -> FoldOperator
    | Serialization.Operator.IfOperator -> IfOperator
    | invalidOpr -> invalidArg "operator" (sprintf "Unsupported value: %A" invalidOpr)

let rec serialize (TaggedNaplExpression (_, expr)) =
    let res = Serialization.Expression()
    match expr with
    | ValueExpression value ->
        res.ExpressionType <- Serialization.ExpressionType.ValueExpression
        match value with
        | BooleanValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.BooleanKind)
            res.boolean_operands.Add(v)
        | IntegerValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.IntegerKind)
            res.integer_operands.Add(v)
        | FloatValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.FloatKind)
            res.float_operands.Add(v)
        | StringValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.StringKind)
            res.string_operands.Add(v)
