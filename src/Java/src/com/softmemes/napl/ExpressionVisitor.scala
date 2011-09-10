package com.softmemes.napl

object ExpressionVisitor {
  def visit[T1,T2] (f : ((T1, NaplExpression[T2]) => Unit), state : T1, expr : NaplExpression[T2]) = {
    expr.expression match {
      case LambdaExpression(param, expr) => f(state, expr)
      case ValueExpression(_) => ()
      case ParameterExpression(_) => ()
      case OperatorExpression(opr, exprs) =>
        for (expr <- exprs) f(state, expr)
      case CollectionExpression(t, exprs) =>
        for (expr <- exprs) f(state, expr)
      case ApplyExpression(funcExpr, exprs) => {
        f (state, funcExpr)
        for (expr <- exprs) f(state, expr)          
      }
    }
  }
}
