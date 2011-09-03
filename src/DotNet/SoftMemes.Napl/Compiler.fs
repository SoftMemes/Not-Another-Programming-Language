module SoftMemes.Napl.Compiler

open System
open System.Reflection
open System.Linq.Expressions
open SoftMemes.Functional
open SoftMemes.Napl.Language
open SoftMemes.Napl.LanguageSugar
type LinqExpression = System.Linq.Expressions.Expression

let private createTupleType =
    let openTs =
        [|
            typeof<Tuple<_>>;
            typeof<Tuple<_,_>>;
            typeof<Tuple<_,_,_>>;
            typeof<Tuple<_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_,_,_>>;
            typeof<Tuple<_,_,_,_,_,_,_,_>>;
        |] |> Array.map (fun t -> t.GetGenericTypeDefinition())
    fun ts ->
        // TODO: Bounds check
        let openT = openTs.[List.length ts - 1]
        openT.MakeGenericType(List.toArray ts)

let private createFuncType =
    let openTs =
        [|
            typeof<Func<_>>;
            typeof<Func<_,_>>;
            typeof<Func<_,_,_>>;
            typeof<Func<_,_,_,_>>;
            typeof<Func<_,_,_,_,_>>;
            typeof<Func<_,_,_,_,_,_>>;
            typeof<Func<_,_,_,_,_,_,_>>;
            typeof<Func<_,_,_,_,_,_,_,_>>;
        |] |> Array.map (fun t -> t.GetGenericTypeDefinition())
    fun ts tret ->
        // TODO: Bounds check
        let openT = openTs.[List.length ts]
        openT.MakeGenericType(List.toArray (List.append ts [tret]))

let rec private getNativeType = function
    | BooleanType -> typeof<bool>
    | StringType -> typeof<string>
    | IntegerType -> typeof<int32>
    | FloatType -> typeof<float>
    | TupleType ts ->
        let nativeTs = List.map getNativeType ts
        createTupleType nativeTs
    | ListType t ->
        let nativeT = getNativeType t
        typeof<List<_>>.MakeGenericType(nativeT)
    | SetType t ->
        let nativeT = getNativeType t
        typeof<Set<_>>.MakeGenericType(nativeT)
    | MapType (tkey, tvalue) ->
        let nativeTKey = getNativeType tkey
        let nativeTValue = getNativeType tvalue
        typeof<Map<_,_>>.MakeGenericType(nativeTKey, nativeTValue)
    | FunctionType (tins, tout) ->
        let nativeTins = tins |> List.map getNativeType
        let nativeTout = getNativeType tout
        createFuncType nativeTins nativeTout

let private getNativeArgs tin =
    match tin with
    | TupleType (ts & [])
    | TupleType (ts & (_::_::_)) ->
        List.map getNativeType ts
    | t -> [getNativeType t]

let private getNativeParam (Parameter(pt, pn)) =
    let nativePt = getNativeType pt
    Expression.Variable(nativePt, pn)

let private initCollection typeName (t : Type) =
    // TODO: Cache
    let fullTypeName = sprintf "Microsoft.FSharp.Collections.%s" typeName
    let t = Type.GetType(fullTypeName, true)
    let mi = t.GetMethod("OfSeq")
    mi.MakeGenericMethod([|t|])

let rec private getTupleItemNative (pe : ParameterExpression) i tupleT =
    // TODO: Cache members ...
    let propertyName = sprintf "Item%i" (i + 1)
    let propertyInfo = pe.Type.GetProperty(propertyName)
    Expression.Property(pe, propertyInfo)    

let private getType (TaggedNaplExpression ((t,_),_)) = t

let rec private compile env (TaggedNaplExpression ((t,_),expr)) : LinqExpression =
    match expr with
    | ValueExpression value ->
        match value with
        | BooleanValue v -> upcast Expression.Constant(v)
        | IntegerValue v -> upcast Expression.Constant(v)
        | FloatValue v -> upcast Expression.Constant(v)
        | StringValue v -> upcast Expression.Constant(v)
    | ParameterExpression p ->
        upcast Map.find p env
    | LetExpression (param, expr, inExpr) ->
        let nativeExpr = compile env expr
        let nativeParam = getNativeParam param
        let env' = Map.add param nativeParam env
        let nativeAssign = Expression.Assign(nativeParam, nativeExpr) :> Expression
        let nativeInExpr = compile env' inExpr
        let nativeT = getNativeType t
        upcast Expression.Block(
            nativeT,
            [|nativeParam|],
            [|nativeAssign;nativeInExpr|])
    | MatchExpression (ps, expr, inExpr) ->
        let nativeExpr = compile env expr
        let (tupleT & TupleType ts) = getType expr
        let nativeTupleT = ts |> List.map getNativeType |> createTupleType
        let tempParam = Expression.Parameter(nativeTupleT)
        let tempAssign = Expression.Assign(tempParam, nativeExpr) :> Expression

        let nativeParams = tempParam :: (ps |> List.map getNativeParam)
        let nativeExtracts =
            nativeParams
            |> List.mapi (getTupleItemNative tempParam)
        let nativeAssigns =
            List.zip nativeParams nativeExtracts
            |> List.map (fun (p,e) -> Expression.Assign(p, e) :> Expression)
        let env' =
            List.zip ps nativeParams
            |> List.fold (fun e (k,v) -> Map.add k v e) env            
        let nativeInExpr = compile env' inExpr
        let nativeT = getNativeType t
        upcast LinqExpression.Block(
            nativeT,
            List.toSeq nativeParams,
            List.concat [[tempAssign];nativeAssigns;[nativeInExpr]])
    | FunctionExpression (ps, expr) ->
        let nativeParams = ps |> List.map getNativeParam
        let nativeParamTs = ps |> List.map (fun (Parameter (t,_)) -> getNativeType t) 
        let env' =
            List.zip ps nativeParams
            |> List.fold (fun e (k,v) -> Map.add k v e) env            
        let nativeT = getNativeType t
        let nativeExpr = compile env' expr
        let nativeFunType = createFuncType nativeParamTs nativeT
        upcast LinqExpression.Lambda(nativeFunType, nativeExpr, nativeParams)
    | CallExpression (funcExpr, paramsExpr) ->
        let nativeFuncExpr = compile env funcExpr
        let nativeParams = paramsExpr |> List.map (compile env)
        upcast LinqExpression.Invoke(nativeFuncExpr, nativeParams)
    | TupleExpression exprs ->
        let nativeExprs = exprs |> List.map (compile env)
        let nativeTs = exprs |> List.map (getType >> getNativeType)
        let nativeTupleT = createTupleType nativeTs
        let nativeCInfo = nativeTupleT.GetConstructor(List.toArray nativeTs)
        upcast LinqExpression.New(nativeCInfo, nativeExprs)        
    | CollectionExpression (t, exprs) ->
        let (CollectionType t') = t
        let nativeExprs = exprs |> List.map (compile env)
        let nativeItemT = getNativeType t'
        let nativeCtor =
            match t with
            | ListType _ -> initCollection "List" nativeItemT
            | SetType _ -> initCollection "Set" nativeItemT
            | MapType _ -> initCollection "Map" nativeItemT
        let nativeArray = Expression.NewArrayInit(nativeItemT, nativeExprs)
        upcast LinqExpression.Call(nativeCtor, nativeArray)
    | OperatorExpression (opr, exprs) -> compileOperator env t opr exprs
and
    private compileOperator env t opr exprs : LinqExpression =
        let exprTs = exprs |> List.map getType
        let nativeTs = exprTs |> List.map getNativeType
        match opr, exprs with
        | BinaryOperator, [leftExpr;rightExpr] ->
            let nativeLeftExpr = compile env leftExpr
            let nativeRightExpr = compile env rightExpr
            match opr with
            | EqualsOperator ->
                upcast LinqExpression.Equal(nativeLeftExpr, nativeRightExpr)
            | NotEqualsOperator ->
                upcast LinqExpression.NotEqual(nativeLeftExpr, nativeRightExpr)
            | LessThanOperator ->
                upcast LinqExpression.LessThan(nativeLeftExpr, nativeRightExpr)
            | LessThanEqualsOperator ->
                upcast LinqExpression.LessThanOrEqual(nativeLeftExpr, nativeRightExpr)
            | GreaterThanOperator ->
                upcast LinqExpression.GreaterThan(nativeLeftExpr, nativeRightExpr)
            | GreaterThanEqualsOperator ->
                upcast LinqExpression.GreaterThanOrEqual(nativeLeftExpr, nativeRightExpr)
            | AndOperator ->
                upcast LinqExpression.And(nativeLeftExpr, nativeRightExpr)
            | OrOperator ->
                upcast LinqExpression.Or(nativeLeftExpr, nativeRightExpr)
            | AddOperator ->
                upcast LinqExpression.Add(nativeLeftExpr, nativeRightExpr)
            | SubtractOperator ->
                upcast LinqExpression.Subtract(nativeLeftExpr, nativeRightExpr)
            | MultiplyOperator ->
                upcast LinqExpression.Multiply(nativeLeftExpr, nativeRightExpr)
            | DivideOperator ->
                upcast LinqExpression.Divide(nativeLeftExpr, nativeRightExpr)
        | IfOperator, [condExpr;trueExpr;falseExpr] ->
            let nativeCondExpr = compile env condExpr
            let nativeTrueExpr = compile env trueExpr
            let nativeFalseExpr = compile env falseExpr
            let nativeT = getNativeType t
            upcast LinqExpression.Condition(
                nativeCondExpr, nativeTrueExpr, nativeFalseExpr, nativeT)
    // TODO: Implement other operators ...

let Compile e = compile Map.empty e
