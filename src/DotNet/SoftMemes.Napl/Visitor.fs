﻿module SoftMemes.Napl.ExpressionVisitor

open SoftMemes.Napl.Language

let visit f state (NaplExpression (_,expr)) =
    match expr with
    | LambdaExpression (param, expr) ->
        f state expr
    | ValueExpression _ -> ()
    | ParameterExpression _ -> ()
    | OperatorExpression (opr, exprs) ->
        exprs |> List.iter (f state)
    | CollectionExpression (t, exprs) ->
        exprs |> List.iter (f state)
    | ApplyExpression (funcExpr, exprs) ->
        f state funcExpr
        exprs |> List.iter (f state)
