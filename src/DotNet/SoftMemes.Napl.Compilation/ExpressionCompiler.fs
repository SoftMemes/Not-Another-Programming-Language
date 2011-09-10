module SoftMemes.Napl.Compilation.ExpressionCompiler

open System.Linq.Expressions

open SoftMemes.Napl
open SoftMemes.Napl.Types
open SoftMemes.Napl.Compilation.ErrorReporter
open SoftMemes.Napl.Compilation.PlatformFunctions
open SoftMemes.Napl.Compilation.TypeCompiler

type LinqExpression = System.Linq.Expressions.Expression

let rec typeCheck (NaplExpression (tag, expr) as expr') =
    let ret t' expr = NaplExpression ((t',tag), expr)
    let extract (NaplExpression ((exprT, _), _) as expr) = exprT, expr
    let typeCheck' = typeCheck >> extract
    match expr with
    | ValueExpression value ->
        match value with
        | BooleanValue _ -> ret BooleanType (ValueExpression value)
        | IntegerValue _ -> ret IntegerType (ValueExpression value)
        | FloatValue _ -> ret FloatType (ValueExpression value)
        | StringValue _ -> ret StringType (ValueExpression value)
    | OperatorExpression (opr, exprs) ->
        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
        let t = OperatorCompiler.typeCheck expr opr ts
        ret t (OperatorExpression (opr, exprs'))
    | InstantiateExpression (t, exprs) ->
        let ts', exprs' = exprs |> List.map typeCheck' |> List.unzip
        match t with
        | CollectionType t' when List.forall (fun t'' -> t' = t'') ts' ->
            ret t (InstantiateExpression (t, exprs'))
        | CollectionType t' ->
            typeError expr (sprintf "collection of %A" t') ts'
        | TupleType ts ->
            if ts = ts' then ret t (InstantiateExpression (t, exprs'))
            else typeError expr (sprintf "%A" ts) ts'            
        | _ -> typeError expr "collection type" ts'
    | LambdaExpression (param, expr) ->
        let exprT, expr' = typeCheck' expr
        ret exprT (LambdaExpression (param, expr'))
    | ParameterExpression (NaplParameter (t, _) as p) ->
        ret t (ParameterExpression p)
    | ApplyExpression (funcExpr, paramExprs) ->
        let funcT, funcExpr' = typeCheck' funcExpr
        let paramTs, paramExprs' = paramExprs |> List.map typeCheck' |> List.unzip
        match funcT with
        | FunctionType (pts, t) when pts = paramTs ->
            ret t (ApplyExpression (funcExpr', paramExprs'))
        | FunctionType (pts, t) ->
            typeError expr (sprintf "%A" pts) paramTs
        | t ->
            typeError expr "function type" paramTs

let private getNativeParam (NaplParameter(pt, pn)) =
    let nativePt = getNativeType pt
    Expression.Variable(nativePt, pn)

let rec compile env (NaplExpression ((t,_),expr)) : LinqExpression =
    let compileOperator = OperatorCompiler.compileOperator (compile env)
    match expr with
    | LambdaExpression (ps, expr) ->
        let nativeParams = ps |> List.map getNativeParam
        let nativeParamTs = ps |> List.map (fun (NaplParameter (t,_)) -> getNativeType t) 
        let env' =
            List.zip ps nativeParams
            |> List.fold (fun e (k,v) -> Map.add k v e) env            
        let nativeT = getNativeType t
        let nativeExpr = compile env' expr
        let nativeFunType = createFuncType nativeParamTs nativeT
        upcast LinqExpression.Lambda(nativeFunType, nativeExpr, nativeParams)
    | ValueExpression value ->
        match value with
        | BooleanValue v -> upcast Expression.Constant(v)
        | IntegerValue v -> upcast Expression.Constant(v)
        | FloatValue v -> upcast Expression.Constant(v)
        | StringValue v -> upcast Expression.Constant(v)
    | ParameterExpression p ->
        upcast Map.find p env
    | OperatorExpression (opr, exprs) ->
        compileOperator t opr exprs
    | InstantiateExpression (t, exprs) ->
        let nativeExprs = exprs |> List.map (compile env)
        match t with
        | CollectionType t' ->
            let nativeItemT = getNativeType t'
            let nativeCtor =
                match t with
                | ListType _ -> initCollection "List" nativeItemT
                | SetType _ -> initCollection "Set" nativeItemT
                | MapType _ -> initCollection "Map" nativeItemT
            let nativeArray = Expression.NewArrayInit(nativeItemT, nativeExprs)
            upcast LinqExpression.Call(nativeCtor, nativeArray)
        | TupleType ts ->
            let nativeTs = exprs |> List.map (getType >> getNativeType)
            let nativeTupleT = createTupleType nativeTs
            let nativeCInfo = nativeTupleT.GetConstructor(List.toArray nativeTs)
            upcast LinqExpression.New(nativeCInfo, nativeExprs)        
    | ApplyExpression (funcExpr, paramsExpr) ->
        let nativeFuncExpr = compile env funcExpr
        let nativeParams = paramsExpr |> List.map (compile env)
        upcast LinqExpression.Invoke(nativeFuncExpr, nativeParams)
