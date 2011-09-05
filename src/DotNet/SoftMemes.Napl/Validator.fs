module SoftMemes.Napl.Validator

open SoftMemes.Napl
open SoftMemes.Napl.Language
open SoftMemes.Napl.LanguageSugar

let private referenceError str = raise(System.Exception(str))
let private typeError expr expected ts =
    // TODO: Pretty print expression, showing only limited depth.
    let message = sprintf "Type error in %A: Expected %s, but found %A" expr expected ts
    raise (System.Exception(message))

let private collectionCastType cType t' =
    match cType, t' with
    | ListType _, _ -> ListType t'
    | SetType _, _ -> SetType t'
    | MapType _, (TupleType [tk;tv]) -> MapType (tk, tv)
    | MapType _, t -> invalidArg "t'" "Type argument must be a tuple for maps"
    | _ -> invalidArg "cType" "Argument is not a collection type"

let rec private referenceCheck env (expr' & NaplExpression (_,expr)) =
    match expr with
    | ParameterExpression p ->
        if not (Set.contains p env)
        then referenceError <| sprintf "Parameter %A is not in scope" p
    | LambdaExpression (ps, expr) ->
        let env' = Set.union env (Set.ofList ps)
        ExpressionVisitor.visit referenceCheck env' expr
    | _ -> ExpressionVisitor.visit referenceCheck env expr'

let private typeCheckOperator expr opr ts =
    match opr with
    | ConditionOperator ->
        match ts with
        | [BooleanType;truet;falset] when truet = falset -> truet
        | _ -> typeError expr "[bool,<T1>,<T2>] where t1 = t2" ts
    | EqualityOperator ->
        match ts with
        | [t1;t2] when t1 = t2 -> t1
        | _ -> typeError expr "[<T1>,<T2>] where t1 = t2" ts
    | OrderingOperator ->
        match ts with
        | [t1 & NumericType; t2 & NumericType] when t1 = t2 -> BooleanType
        | _ -> typeError expr "[<T1>,<T2>] where t1 is numeric, t1 = t2" ts
    | LogicOperator ->
        match ts with
        | [BooleanType; BooleanType] -> BooleanType
        | _ -> typeError expr "[bool,bool]" ts
    | NumericOperator ->
        match opr, ts with
        | UnaryOperator, [t & NumericType] -> t
        | UnaryOperator, _ -> typeError expr "[<T>] where T is numeric" ts
        | BinaryOperator, [t1 & NumericType; t2 & NumericType] when t1 = t2 -> t1
        | BinaryOperator, _ -> typeError expr "[<T1>,<T2>] where T1 is numeric, t1 = t2" ts
    | TupleOperator -> TupleType ts
    | CurryOperator ->
        match ts with
        | [FunctionType ([TupleType ts],tret)] -> FunctionType (ts, tret)
        | _ -> typeError expr "<T1> -> <T2> where T1 is tuple" ts
    | UncurryOperator ->
        match ts with
        | [FunctionType (ts, tret)] -> FunctionType ([TupleType ts], tret)
        | _ -> typeError expr "function" ts
    | AppendOperator
    | RemoveOperator ->
        match ts with 
        | [t; CollectionOfType t'] when t = t' -> t
        | _ -> typeError expr "[<T1>,collection of <T1>]" ts
    | UnionOperator
    | MinusOperator ->
        match ts with
        | [tc & CollectionOfType t1; CollectionOfType t2] when t1 = t2 -> tc
        | _ -> typeError expr "[collection of <T1>,collection of <T1>]" ts
    | ZipOperator ->
        match ts with
        | [CollectionOfType t1; CollectionOfType t2] ->
            TupleType [t1;t2] |> ListType |> CollectionType
        | _ -> typeError expr "[collection of of <T1>,collection of <T2>]" ts
    | ContainsOperator ->
        match ts with 
        | [t; ListOfType t']
        | [t; SetOfType t']
        | [t; MapOfKeyType t'] -> BooleanType
        | _ -> typeError expr "[<T>,collection of <T>]" ts
    | LookupOperator ->
        match ts with 
        | [t; ListOfType t'] when t = IntegerType -> t'
        | [_; ListOfType _] -> typeError expr "[integer,list of <T>]" ts
        | [t; SetOfType t'] when t = t' -> BooleanType
        | [_; SetOfType _] -> typeError expr "[<T>,set of <T>]" ts
        | [t; MapOfType (tk, tv)] when t = tk -> tv
        | [_; MapOfType _] -> typeError expr "[<T1>,map of <T1>,<T2>]" ts
        | _ -> typeError expr "[<T1>,<T2>]" ts
    | FoldOperator ->
        match ts with
        | [FunctionType ([at], t);at';CollectionOfType t'] when at = at' && t = t' -> at
        | _ -> typeError expr "[(<T1>, <T2>) -> <T1>,<T1>,collection of <T2>]" ts

let rec private typeCheck (expr' & (NaplExpression (tag, expr))) =
    let ret t' expr = NaplExpression ((t',tag), expr)
    let extract (expr & NaplExpression ((exprT, _), _)) = exprT, expr
    let typeCheck' = typeCheck >> extract
    match expr with
    | LambdaExpression (param, expr) ->
        let exprT, expr' = typeCheck' expr
        ret exprT (LambdaExpression (param, expr'))
    | ValueExpression value ->
        match value with
        | BooleanValue _ -> ret BooleanType (ValueExpression value)
        | IntegerValue _ -> ret IntegerType (ValueExpression value)
        | FloatValue _ -> ret FloatType (ValueExpression value)
        | StringValue _ -> ret StringType (ValueExpression value)
    | ParameterExpression (p & Parameter (t, _)) ->
        ret t (ParameterExpression p)
    | OperatorExpression (opr, exprs) ->
        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
        let t = typeCheckOperator expr opr ts
        ret t (OperatorExpression (opr, exprs'))
    | CollectionExpression (t, exprs) ->
        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
        match t with
        | CollectionTypeOf t' when List.forall (fun t'' -> t' = t'') ts ->
            ret (CollectionType t) (CollectionExpression (t, exprs'))
        | CollectionTypeOf t' ->
            typeError expr (sprintf "collection of %A" t') ts
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

let TypeCheck e =
    referenceCheck Set.empty e
    typeCheck e
