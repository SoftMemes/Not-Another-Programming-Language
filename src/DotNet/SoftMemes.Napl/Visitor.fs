module SoftMemes.Napl.ExpressionVisitor

open SoftMemes.Napl.Language

let visit f state (TaggedNaplExpression (_,expr)) =
    match expr with
    | ValueExpression _ -> ()
    | ParameterExpression _ -> ()
    | LetExpression (param, expr, inExpr) ->
        f state expr
        f state inExpr
    | MatchExpression (ps, expr, inExpr) ->
        f state expr
        f state inExpr
    | LambdaExpression (param, expr) ->
        f state expr
    | CallExpression (funcExpr, paramExprs) ->
        f state funcExpr
        paramExprs |> List.iter (f state)
    | TupleExpression exprs ->
        exprs |> List.iter (f state)
    | CollectionExpression (t, exprs) ->
        exprs |> List.iter (f state)
    | OperatorExpression (opr, exprs) ->
        exprs |> List.iter (f state)
