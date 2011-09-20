namespace SoftMemes.Napl

module NaplExpressionBuilder =
    let private Tag e = { NaplExpression.Annotation = Empty; Expression = e}
    // Primitive expressions
    let Lambda ps expr = LambdaExpression (List.ofSeq ps, expr) |> Tag
    let Value v = v |> ValueExpression |> Tag
    let Parameter param = param |> ParameterExpression |> Tag
    let Operator opr exprs = OperatorExpression (opr, exprs) |> Tag
    let Apply funcExpr paramExprs = ApplyExpression (funcExpr, paramExprs) |> Tag
    let New t exprs = InstantiateExpression (t, exprs) |> Tag
    // Shortcuts for literals
    let BooleanValue v = v |> BooleanValue |> Value
    let True = BooleanValue true
    let False = BooleanValue false
    let IntegerValue v = v |> IntegerValue |> Value
    let FloatValue v = v |> FloatValue |> Value
    let StringValue v = v |> StringValue |> Value
    let Tuple ts exprs = InstantiateExpression(TupleType(ts), exprs) |> Tag
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

    let rec Untag {NaplExpression.Expression = expr} =
        let expr' =
            match expr with
            | LambdaExpression (param, expr) ->
                LambdaExpression (param, Untag expr)
            | ValueExpression v -> ValueExpression v
            | ParameterExpression p -> ParameterExpression p
            | OperatorExpression (opr, exprs) ->
                OperatorExpression (opr, exprs |> List.map Untag)
            | ApplyExpression (funcExpr, exprs) ->
                ApplyExpression (Untag funcExpr, exprs |> List.map Untag)
            | InstantiateExpression (t, exprs) ->
                InstantiateExpression (t, exprs |> List.map Untag)
        {NaplExpression.Expression = expr'; Annotation = Unit.Empty}

module TaggedNaplExpressionBuilder =
    let private Tag e t = { NaplExpression.Annotation = t; Expression = e}
    // Primitive expressions
    let Lambda ps expr = LambdaExpression (List.ofSeq ps, expr) |> Tag
    let Value v = v |> ValueExpression |> Tag
    let Parameter param = param |> ParameterExpression |> Tag
    let Operator opr exprs = OperatorExpression (opr, exprs) |> Tag
    let Apply funcExpr paramExprs = ApplyExpression (funcExpr, paramExprs) |> Tag
    let New t exprs = InstantiateExpression (t, exprs) |> Tag
    // Shortcuts for literals
    let BooleanValue v = v |> BooleanValue |> Value
    let True t = BooleanValue true t
    let False t = BooleanValue false t
    let IntegerValue v = v |> IntegerValue |> Value
    let FloatValue v = v |> FloatValue |> Value
    let StringValue v = v |> StringValue |> Value
    let Tuple ts exprs = InstantiateExpression(TupleType(ts), exprs) |> Tag
    // Operator shortcuts
    let UnaryOperator opr expr = OperatorExpression (opr, [expr]) |> Tag
    let BinaryOperator opr leftExpr rightExpr = Operator opr [leftExpr;rightExpr]
    // Higher order expressions
    let Let param expr inExpr t = Apply (Lambda [param] inExpr t) [expr] t
    let Curry fexpr = Operator CurryOperator [fexpr]
    let Uncurry fexpr = Operator UncurryOperator [fexpr]
    let Match ps expr inExpr t = Apply (Curry (Lambda [ps] inExpr t) t) [expr] t
    // Control flow
    let If condExpr trueExpr falseExpr = Operator ConditionOperator [condExpr;trueExpr;falseExpr]
