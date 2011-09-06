module  SoftMemes.Napl.Serializer

open SoftMemes.Functional
open SoftMemes.Napl
open SoftMemes.Napl.Language

let private serializeOperator =
    function
    | ConditionOperator -> Serialization.Operator.ConditionOperator
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
    | TupleOperator -> Serialization.Operator.TupleOperator
    | CurryOperator -> Serialization.Operator.CurryOperator
    | UncurryOperator -> Serialization.Operator.UncurryOperator
    | AppendOperator -> Serialization.Operator.AppendOperator
    | RemoveOperator -> Serialization.Operator.RemoveOperator
    | UnionOperator -> Serialization.Operator.UnionOperator
    | MinusOperator -> Serialization.Operator.MinusOperator
    | ZipOperator -> Serialization.Operator.ZipOperator
    | ContainsOperator -> Serialization.Operator.ContainsOperator
    | LookupOperator -> Serialization.Operator.LookupOperator
    | FoldOperator -> Serialization.Operator.FoldOperator

let private deserializeOperator =
    function
    | Serialization.Operator.ConditionOperator -> ConditionOperator
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
    | Serialization.Operator.TupleOperator -> TupleOperator
    | Serialization.Operator.CurryOperator -> CurryOperator
    | Serialization.Operator.UncurryOperator -> UncurryOperator
    | Serialization.Operator.AppendOperator -> AppendOperator
    | Serialization.Operator.RemoveOperator -> RemoveOperator
    | Serialization.Operator.UnionOperator -> UnionOperator
    | Serialization.Operator.MinusOperator -> MinusOperator
    | Serialization.Operator.ZipOperator -> ZipOperator
    | Serialization.Operator.ContainsOperator -> ContainsOperator
    | Serialization.Operator.LookupOperator -> LookupOperator
    | Serialization.Operator.FoldOperator -> FoldOperator
    | invalidOpr -> invalidArg "operator" (sprintf "Unsupported value: %A" invalidOpr)

let rec private serializeNaplType t =
    let res = Serialization.NaplType()
    match t with
    | BooleanType -> res.kind <- Serialization.NaplValueKind.BooleanKind
    | StringType -> res.kind <- Serialization.NaplValueKind.StringKind
    | IntegerType -> res.kind <- Serialization.NaplValueKind.IntegerKind
    | FloatType -> res.kind <- Serialization.NaplValueKind.FloatKind
    | TupleType ts ->
        res.kind <- Serialization.NaplValueKind.TupleKind
        res.sub_types.AddRange(ts |> List.map serializeNaplType)
    | CollectionType(ListType t) ->
        res.kind <- Serialization.NaplValueKind.ListKind
        res.sub_types.Add(serializeNaplType t)
    | CollectionType(SetType t) ->
        res.kind <- Serialization.NaplValueKind.SetKind
        res.sub_types.Add(serializeNaplType t)
    | CollectionType(MapType (tk, tv)) ->
        res.kind <- Serialization.NaplValueKind.MapKind
        res.sub_types.Add(serializeNaplType tk)
        res.sub_types.Add(serializeNaplType tv)
    | FunctionType (ts, t) ->
        res.kind <- Serialization.NaplValueKind.FunctionKind
        res.sub_types.Add(serializeNaplType t)
        res.sub_types.AddRange(ts |> List.map serializeNaplType)
    res

let private serializeParameter (NaplParameter(pt, name)) pi =
    let res = Serialization.Parameter()
    res.param_type <- serializeNaplType pt
    res.id <- pi
    res.name <- name
    res

let private lookupParam env p = let (l,_) = env in Map.find p l
let private rememberParam env p =
    let (l,i) = env
    let i' = 1 + i
    Map.add p i' l, i'

let private serializeParameters env ps =
    let envs = ps |> List.scan rememberParam env |> List.tail
    let env' = envs |> List.rev |> List.head
    let pis = envs |> List.map snd
    let paramMsgs =
        List.zip ps pis
        |> List.map (uncurry serializeParameter)
    env', paramMsgs

let rec private serialize env (NaplExpression (_, expr)) =
    let res = Serialization.Expression()
    match expr with
    | LambdaExpression (ps, expr) ->
        let env', paramMsgs = serializeParameters env ps
        res.expression_type <- Serialization.ExpressionType.LambdaExpression
        res.parameter_operands.AddRange(paramMsgs)
        res.expression_operands.Add(serialize env' expr)
    | ValueExpression value ->
        res.expression_type <- Serialization.ExpressionType.ValueExpression
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
    | ParameterExpression (NaplParameter(t, name) as param)->
        let paramIdx = lookupParam env param
        res.expression_type <- Serialization.ExpressionType.ParameterExpression
        res.parameter_reference_operands.Add(paramIdx)
    | OperatorExpression (opr, exprs) ->
        res.expression_type <- Serialization.ExpressionType.CollectionExpression
        res.operator_operands.Add(serializeOperator opr)
        res.expression_operands.AddRange(exprs |> List.map (serialize env))
    | CollectionExpression (t, exprs) ->
        res.expression_type <- Serialization.ExpressionType.CollectionExpression
        res.value_type_operands.Add(serializeNaplType (CollectionType t))
        res.expression_operands.AddRange(exprs |> List.map (serialize env))
    | ApplyExpression (fExpr, pExprs) ->
        res.expression_type <- Serialization.ExpressionType.ApplyExpression
        res.expression_operands.Add(serialize env fExpr)
        res.expression_operands.AddRange(pExprs |> List.map (serialize env))
    res

let Serialize e = serialize (Map.empty, 0) e
