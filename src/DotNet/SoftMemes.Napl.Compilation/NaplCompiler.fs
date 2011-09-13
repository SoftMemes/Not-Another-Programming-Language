module SoftMemes.Napl.NaplCompiler

open SoftMemes.Napl
open SoftMemes.Napl.Compilation
open SoftMemes.Napl.Compilation.ErrorReporter

let rec private referenceCheck env ({NaplExpression.Expression = expr} as expr') =
    match expr with
    | ParameterExpression p ->
        if not (Set.contains p env)
        then referenceError expr' <| sprintf "Parameter %A is not in scope" p
    | LambdaExpression (ps, expr) ->
        let env' = Set.union env (Set.ofList ps)
        ExpressionVisitor.visit referenceCheck env' expr
    | _ -> ExpressionVisitor.visit referenceCheck env expr'

let TypeCheck e =
    referenceCheck Set.empty e
    ExpressionCompiler.typeCheck e

let Compile e =
    referenceCheck Set.empty e
    let typedE = ExpressionCompiler.typeCheck e
    ExpressionCompiler.compile Map.empty typedE
