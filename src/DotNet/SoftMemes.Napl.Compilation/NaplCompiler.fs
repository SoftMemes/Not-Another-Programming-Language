module SoftMemes.Napl.NaplCompiler

type LinqExpression = System.Linq.Expressions.Expression
type LinqLambdaExpression = System.Linq.Expressions.LambdaExpression

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
    let linqExpr = ExpressionCompiler.compile Map.empty typedE
    // TODO: Move somewhere sensible or rethink
    match linqExpr with
    | :? LinqLambdaExpression as linqLambda -> linqLambda
    | linqExpr -> LinqExpression.Lambda(linqExpr, [||])

let GetClrType t = TypeCompiler.getNativeType t
