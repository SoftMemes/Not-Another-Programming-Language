module SoftMemes.Napl.Validator

open SoftMemes.Napl
open SoftMemes.Napl.Language
open SoftMemes.Napl.LanguageSugar

exception TypeError of string
exception ReferenceError of string

let private referenceError str = raise(System.Exception(str))
let private typeError str = raise (System.Exception(str))

let private collectionCastType cType t' =
    match cType, t' with
    | ListType _, _ -> ListType t'
    | SetType _, _ -> SetType t'
    | MapType _, (TupleType [tk;tv]) -> MapType (tk, tv)
    | MapType _, t -> invalidArg "t'" "Type argument must be a tuple for maps"
    | _ -> invalidArg "cType" "Argument is not a collection type"

let rec private referenceCheck env (expr' & TaggedNaplExpression (_,expr)) =
    match expr with
    | ParameterExpression p ->
        if not (Set.contains p env)
        then referenceError <| sprintf "Parameter %A is not in scope" p
    | LetExpression (param, expr, inExpr) ->
        let env' = Set.add param env
        ExpressionVisitor.visit referenceCheck env' expr
    | MatchExpression (ps, expr, inExpr) ->
        let env' = Set.union env (Set.ofList ps)
        ExpressionVisitor.visit referenceCheck env' expr
    | FunctionExpression (ps, expr) ->
        let env' = Set.union env (Set.ofList ps)
        ExpressionVisitor.visit referenceCheck env' expr
    | _ -> ExpressionVisitor.visit referenceCheck env expr'

let private typeCheckOperator opr ts =
    let expectedTs =
        match opr with
        | UnaryOperator -> 1
        | BinaryOperator -> 2
        | TrinaryOperator -> 3
    if expectedTs <> List.length ts then
        typeError <| sprintf "Operator %A expected %i arguments, but given %i" opr expectedTs (List.length ts)
    match opr, ts with
    | EqualityOperator, [t1;t2] ->
        if t1 = t2 then t1
        else typeError <| sprintf "Type %A does not match type %A" t1 t2
    | OrderingOperator, [t1;t2] ->
        match t1, t2 with
        | NumericType, NumericType when t1 = t2 -> BooleanType
        | NumericType, NumericType ->
            typeError <| sprintf "Type %A does not match type %A" t1 t2
        | t1, t2 ->
            typeError <| sprintf "Order comparisons are only valid on numeric types"            
    | LogicOperator, [t1;t2] ->
        match t1, t2 with
        | BooleanType, BooleanType -> BooleanType
        | _ -> typeError <| sprintf "Logic operators expects boolean types but were given %A and %A" t1 t2
    | NumericOperator & UnaryOperator, [t] ->
        match t with
        | NumericType -> t
        | t -> typeError <| sprintf "Expected numeric type, found %A" t
    | NumericOperator & BinaryOperator, [t1;t2] ->
        match t1, t2 with
        | NumericType, NumericType when t1 = t2 -> t1
        | NumericType, NumericType -> typeError <| sprintf "Type %A does not match type %A" t1 t2
        | t1, t2 -> typeError <| sprintf "Expected numeric types, found %A and %A" t1 t2
    | AppendOperator, [t; tcol]
    | RemoveOperator, [t; tcol] ->
        match tcol with 
        | CollectionType t' when t = t' -> tcol
        | CollectionType t' ->
            typeError <| sprintf "Type %A is not compatible with collection of type %A" t tcol
        | tcol ->
            typeError <| sprintf "Expected collection type, found %A" tcol
    | UnionOperator, [tc1;tc2]
    | MinusOperator, [tc1;tc2] ->
        match tc1, tc2 with
        | CollectionType t1, CollectionType t2 when t1 = t2 -> tc1
        | CollectionType t1, CollectionType t2 ->
            typeError <| sprintf "Collection of %A is not compatible with collection of %A" t1 t2
        | t1, t2 ->
            typeError <| sprintf "Expected collection types, found %A and %A" t1 t2
    | ZipOperator, [tc1;tc2] ->
        match tc1, tc2 with
        | ListType t1, ListType t2 -> ListType (TupleType [t1;t2])
        | SetType t1, SetType t2 -> SetType (TupleType [t1;t2])
        | MapType (tk1, tv1), MapType (tk2, tv2) -> MapType (TupleType [tk1;tk2], TupleType [tv1;tv2])
        | t1, t2 -> typeError <| sprintf "Expected compabitle collections, fond %A and %A" t1 t2
    | ContainsOperator, [t; tcol] ->
        match tcol with 
        | ListType t'
        | SetType t'
        | MapType (t', _) ->
            if t = t' then BooleanType
            else typeError <| sprintf "Expected type %A for contains but found %A" t' t
        | t ->
            typeError <| sprintf "Expected collection type, found %A" t
    | LookupOperator, [t; tcol] ->
        match tcol with 
        | ListType t' ->
            match t with
            | IntegerType -> t'
            | t -> typeError <| sprintf "List lookup requires integer index, was given %A" t
        | SetType t' ->
            if t = t' then BooleanType
            else typeError <| sprintf "Expected type %A for set lookup but found %A" t' t
        | MapType (tk, tv) ->
            if t = tk then tv
            else typeError <| sprintf "Expected type %A for map lookup but found %A" tk t
        | t ->
            typeError <| sprintf "Expected collection type, found %A" t
    | FoldOperator, [ft;at;ct] -> 
        match ct with
        | CollectionType t' ->
            let expectedFT = FunctionType([at;t'], at)
            if ft = expectedFT then at
            else typeError <| sprintf "Expected %A, found %A" expectedFT ft
        | t -> typeError <| sprintf "Expected collection type, found %A" t
    | IfOperator, [condt;truet;falset] ->
        match condt, truet, falset with
        | BooleanType, trueType, falseType when trueType = falseType -> trueType
        | BooleanType, trueType, falseType -> 
            typeError <| sprintf "Type mismatch in if expression, %A does not match %A" trueType falseType
        | condType, _, _ ->
            typeError <| sprintf "Condition expected to be of type boolean, was type %A" condType

let rec private typeCheck (TaggedNaplExpression (tag, expr)) =
    let ret t' expr = TaggedNaplExpression ((t',tag), expr)
    let extract (expr & TaggedNaplExpression ((exprT, _), _)) = exprT, expr
    let typeCheck' = typeCheck >> extract
    match expr with
    | ValueExpression value ->
        match value with
        | BooleanValue _ -> ret BooleanType (ValueExpression value)
        | IntegerValue _ -> ret IntegerType (ValueExpression value)
        | FloatValue _ -> ret FloatType (ValueExpression value)
        | StringValue _ -> ret StringType (ValueExpression value)
    | ParameterExpression (p & Parameter (t, _)) ->
        ret t (ParameterExpression p)
    | LetExpression (param, expr, inExpr) ->
        let exprT, expr' = typeCheck' expr
        let inExprT, inExpr' = typeCheck' inExpr
        match param with
        | Parameter(pType, pName) when exprT = pType ->
            ret inExprT (LetExpression (param, expr', inExpr'))
        | Parameter(pType, pName) -> 
            typeError <| sprintf "Cannot assign expression of type %A to parameter %s of type %A" exprT pName pType
    | MatchExpression (ps, expr, inExpr) ->
        let exprT, expr' = typeCheck' expr
        let inExprT, inExpr' = typeCheck' inExpr
        match List.map (fun (Parameter (t, _)) -> t) ps, exprT with
        | ts, (TupleType ts') when ts = ts' ->
            ret inExprT (MatchExpression (ps, expr', inExpr'))
        | ts, (TupleType ts') ->
            typeError <| sprintf "Type list %A doesn't match expected %A" ts' ts
        | ts, t ->
            typeError <| sprintf "Match expects a tuple but found %A" t
    | FunctionExpression (param, expr) ->
        let exprT, expr' = typeCheck' expr
        ret exprT (FunctionExpression (param, expr'))
    | CallExpression (funcExpr, paramExprs) ->
        let funcT, funcExpr' = typeCheck' funcExpr
        let paramTs, paramExprs' = paramExprs |> List.map typeCheck' |> List.unzip
        match funcT with
        | FunctionType (pts, t) when pts = paramTs ->
            ret t (CallExpression (funcExpr', paramExprs'))
        | FunctionType (pts, t) ->
            typeError <| sprintf "Provided parameters of %A does not match expected of %A" paramTs pts
        | t ->
            typeError <| sprintf "Expected function, found %A" t
    | TupleExpression exprs ->
        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
        ret (TupleType ts) (TupleExpression exprs')
    | CollectionExpression (t, exprs) ->
        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
        match t with
        | CollectionType t' when List.forall (fun t'' -> t' = t'') ts ->
            ret t (CollectionExpression (t, exprs'))
        | CollectionType t' ->
            typeError <| sprintf "Expected collection with items of type %A" t'
        | _ ->
            typeError <| sprintf "Expected collection type, found %A" t
    | OperatorExpression (opr, exprs) ->
        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
        let t = typeCheckOperator opr ts
        ret t (OperatorExpression (opr, exprs'))

let TypeCheck e =
    referenceCheck Set.empty e
    typeCheck e
