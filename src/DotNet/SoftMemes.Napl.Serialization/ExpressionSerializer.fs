module SoftMemes.Napl.Serialization.ExpressionSerializer

open SoftMemes.Napl
open SoftMemes.Napl.Serialization.ErrorReporter

let curry f x y = f (x, y)
let uncurry f (x,y) = f x y

let private lookupParam env p = let (l,_) = env in Map.find p l
let private rememberParam env p =
    let (l,i) = env
    let i' = 1 + i
    Map.add p i' l, i'

let private serializeParameter {NaplParameter.Type = pt; Name = name} pi =
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

let rec serialize env {NaplExpression.Expression = expr} =
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
            res.value_kind_operands.Add(Serialization.NaplValueKind.BooleanValueKind)
            res.boolean_operands.Add(v)
        | IntegerValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.IntegerValueKind)
            res.integer_operands.Add(v)
        | FloatValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.FloatValueKind)
            res.float_operands.Add(v)
        | StringValue v ->
            res.value_kind_operands.Add(Serialization.NaplValueKind.StringValueKind)
            res.string_operands.Add(v)
    | ParameterExpression ({NaplParameter.Type = t; Name = name} as param) ->
        let paramIdx = lookupParam env param
        res.expression_type <- Serialization.NaplExpressionType.ParameterExpression
        res.parameter_reference_operands.Add(paramIdx)
    | OperatorExpression (opr, exprs) ->
        res.expression_type <- Serialization.NaplExpressionType.OperatorExpression
        res.operator_operands.Add(OperatorSerializer.serialize opr)
        res.expression_operands.AddRange(exprs |> List.map (serialize env))
    | InstantiateExpression (t, exprs) ->
        res.expression_type <- Serialization.NaplExpressionType.InstantiateExpression
        res.type_operands.Add(TypeSerializer.serialize t)
        res.expression_operands.AddRange(exprs |> List.map (serialize env))
    | ApplyExpression (fExpr, pExprs) ->
        res.expression_type <- Serialization.NaplExpressionType.ApplyExpression
        res.expression_operands.Add(serialize env fExpr)
        res.expression_operands.AddRange(pExprs |> List.map (serialize env))
    res

let private deserializeParameterRef (env : Map<_,NaplParameter>) (pid : int) =
    match Map.tryFind pid env with
    | Some(param) -> param
    | None -> serializationError <| sprintf "Invalid parameter reference, id: %i" pid

let private deserializeParameter env (p : Serialization.NaplParameter) =
    let naplType = TypeSerializer.deserialize p.param_type
    let p' = {NaplParameter.Type = naplType; Name = p.name}
    let env' = Map.add p.id p' env
    (env', p')

let private deserializeParameters env ps =
    let deserializeOne (env,ps) p =
        let env', p' = deserializeParameter env p
        env', p' :: ps
        
    let env', ps' = ps |> List.fold deserializeOne (env,[])
    env', ps' |> List.rev

let rec deserialize env (expr : Serialization.NaplExpression) =
    let ret expr' = {NaplExpression.Annotation = expr; Expression = expr'}
    match expr.expression_type with
    | Serialization.NaplExpressionType.LambdaExpression ->
        let env', ps = expr.parameter_operands |> List.ofSeq |> deserializeParameters env 
        let bodyExpr = expr.expression_operands.[0] |> deserialize env'
        ret <| LambdaExpression(ps, bodyExpr)
    | Serialization.NaplExpressionType.ValueExpression ->
        let expr' =
            match List.ofSeq expr.value_kind_operands with
            | [Serialization.NaplValueKind.BooleanValueKind] ->
                expr.boolean_operands.[0] |> BooleanValue |> ValueExpression
            | [Serialization.NaplValueKind.IntegerValueKind] ->
                expr.integer_operands.[0] |> IntegerValue |> ValueExpression
            | [Serialization.NaplValueKind.FloatValueKind] ->
                expr.float_operands.[0] |> FloatValue |> ValueExpression
            | [Serialization.NaplValueKind.StringValueKind] ->
                expr.string_operands.[0] |> StringValue |> ValueExpression
            | _ -> serializationError "Invalid number of value kind parameters in value expression"
        ret expr'
    | Serialization.NaplExpressionType.ParameterExpression ->
        let naplParam = deserializeParameterRef env expr.parameter_reference_operands.[0]
        ret <| ParameterExpression(naplParam)
    | Serialization.NaplExpressionType.OperatorExpression ->
        let opr = OperatorSerializer.deserialize expr.operator_operands.[0]
        let exprs' = expr.expression_operands |> List.ofSeq |> List.map (deserialize env)
        ret <| OperatorExpression(opr, exprs')
    | Serialization.NaplExpressionType.InstantiateExpression ->
        let t = TypeSerializer.deserialize expr.type_operands.[0]
        let exprs' = expr.expression_operands |> List.ofSeq |> List.map (deserialize env)
        ret <| InstantiateExpression(t, exprs')
    | Serialization.NaplExpressionType.ApplyExpression ->
        let fExpr = expr.expression_operands |> Seq.head |> deserialize env
        let exprs' = expr.expression_operands |> List.ofSeq |> List.tail |> List.map (deserialize env)
        ret <| ApplyExpression(fExpr, exprs')

