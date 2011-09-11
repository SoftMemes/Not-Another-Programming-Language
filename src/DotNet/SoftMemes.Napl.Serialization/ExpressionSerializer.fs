module SoftMemes.Napl.Serialization.ExpressionSerializer

open SoftMemes.Napl

let curry f x y = f (x, y)
let uncurry f (x,y) = f x y

let private lookupParam env p = let (l,_) = env in Map.find p l
let private rememberParam env p =
    let (l,i) = env
    let i' = 1 + i
    Map.add p i' l, i'

let private serializeParameter (NaplParameter(pt, name)) pi =
    let res = Serialization.NaplParameter()
    res.param_type <- TypeSerializer.serialize pt
    res.id <- pi
    res.name <- name
    res

let private serializeParameters env ps =
    let envs = ps |> List.scan rememberParam env |> List.tail
    let env' = envs |> List.rev |> List.head
    let pis = envs |> List.map snd
    let paramMsgs =
        List.zip ps pis
        |> List.map (uncurry serializeParameter)
    env', paramMsgs

let rec serialize env (NaplExpression (_, expr)) =
    let res = Serialization.NaplExpression()
    match expr with
    | LambdaExpression (ps, expr) ->
        let env', paramMsgs = serializeParameters env ps
        res.expression_type <- Serialization.NaplExpressionType.LambdaExpression
        res.parameter_operands.AddRange(paramMsgs)
        res.expression_operands.Add(serialize env' expr)
    | ValueExpression value ->
        res.expression_type <- Serialization.NaplExpressionType.ValueExpression
        match value with
        | BooleanValue v ->
            res.value_kind_operands.Add(Serialization.NaplTypeKind.BooleanKind)
            res.boolean_operands.Add(v)
        | IntegerValue v ->
            res.value_kind_operands.Add(Serialization.NaplTypeKind.IntegerKind)
            res.integer_operands.Add(v)
        | FloatValue v ->
            res.value_kind_operands.Add(Serialization.NaplTypeKind.FloatKind)
            res.float_operands.Add(v)
        | StringValue v ->
            res.value_kind_operands.Add(Serialization.NaplTypeKind.StringKind)
            res.string_operands.Add(v)
    | ParameterExpression (NaplParameter(t, name) as param)->
        let paramIdx = lookupParam env param
        res.expression_type <- Serialization.NaplExpressionType.ParameterExpression
        res.parameter_reference_operands.Add(paramIdx)
    | OperatorExpression (opr, exprs) ->
        res.expression_type <- Serialization.NaplExpressionType.OperatorExpression
        res.operator_operands.Add(OperatorSerializer.serialize opr)
        res.expression_operands.AddRange(exprs |> List.map (serialize env))
    | InstantiateExpression (t, exprs) ->
        res.expression_type <- Serialization.NaplExpressionType.InstantiateExpression
        res.value_type_operands.Add(TypeSerializer.serialize t)
        res.expression_operands.AddRange(exprs |> List.map (serialize env))
    | ApplyExpression (fExpr, pExprs) ->
        res.expression_type <- Serialization.NaplExpressionType.ApplyExpression
        res.expression_operands.Add(serialize env fExpr)
        res.expression_operands.AddRange(pExprs |> List.map (serialize env))
    res

