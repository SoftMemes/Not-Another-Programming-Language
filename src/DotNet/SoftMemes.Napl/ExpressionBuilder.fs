namespace SoftMemes.Napl

open SoftMemes.Napl.Language

module Expression =
    let Tag e = TaggedNaplExpression ((), e)
    let Value v = v |> ValueExpression |> Tag
    let BooleanValue v = v |> BooleanValue |> Value
    let True = BooleanValue true
    let False = BooleanValue false
    let IntegerValue v = v |> IntegerValue |> Value
    let FloatValue v = v |> FloatValue |> Value
    let StringValue v = v |> StringValue |> Value
    let Tuple exprs = exprs |> TupleExpression |> Tag
    let Parameter param = param |> ParameterExpression |> Tag
    let Let param expr inExpr = LetExpression (param, expr, inExpr) |> Tag
    let Match ps expr inExpr = MatchExpression (List.ofSeq ps, expr, inExpr) |> Tag
    let Lambda ps expr = LambdaExpression (List.ofSeq ps, expr) |> Tag
    let Call funcExpr paramExprs = CallExpression (funcExpr, paramExprs) |> Tag
    let Collection cType itemsExpr = CollectionExpression (cType, itemsExpr) |> Tag
    let Operator opr exprs = OperatorExpression (opr, exprs) |> Tag
    let UnaryOperator opr expr = OperatorExpression (opr, [expr]) |> Tag
    let BinaryOperator opr leftExpr rightExpr =
        OperatorExpression (opr, [leftExpr;rightExpr]) |> Tag
    let If condExpr trueExpr falseExpr =
        OperatorExpression (IfOperator, [condExpr;trueExpr;falseExpr]) |> Tag
