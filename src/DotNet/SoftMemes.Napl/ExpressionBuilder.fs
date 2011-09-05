namespace SoftMemes.Napl

open SoftMemes.Napl.Language

module Expression =
    let private Tag e = NaplExpression ((), e)
    // Primitive expressions
    let Lambda ps expr = LambdaExpression (List.ofSeq ps, expr) |> Tag
    let Value v = v |> ValueExpression |> Tag
    let Parameter param = param |> ParameterExpression |> Tag
    let Operator opr exprs = OperatorExpression (opr, exprs) |> Tag
    let Collection cType itemsExpr = CollectionExpression (cType, itemsExpr) |> Tag
    let Apply funcExpr paramExprs = ApplyExpression (funcExpr, paramExprs) |> Tag
    // Shortcuts for literals
    let BooleanValue v = v |> BooleanValue |> Value
    let True = BooleanValue true
    let False = BooleanValue false
    let IntegerValue v = v |> IntegerValue |> Value
    let FloatValue v = v |> FloatValue |> Value
    let StringValue v = v |> StringValue |> Value
    let Tuple exprs = exprs |> Operator TupleOperator
    // Operator shortcuts
    let UnaryOperator opr expr = OperatorExpression (opr, [expr]) |> Tag
    let BinaryOperator opr leftExpr rightExpr = Operator opr [leftExpr;rightExpr]
    // Higher order expressions
    let Let param expr inExpr = Apply (Lambda [param] inExpr) [expr]
    let Curry fexpr = Operator CurryOperator [fexpr]
    let Uncurry fexpr = Operator UncurryOperator [fexpr]
    let Match ps expr inExpr = Apply (Curry (Lambda [ps] inExpr)) [expr]
    // Control flow
    let If condExpr trueExpr falseExpr = Operator ConditionOperator [condExpr;trueExpr;falseExpr]
