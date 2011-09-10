package com.softmemes.napl

abstract class NaplValue
case class BooleanValue(value : Boolean) extends NaplValue
case class IntegerValue(value : Int) extends NaplValue
case class FloatValue(value : Double) extends NaplValue
case class StringValue(value : String) extends NaplValue

// TODO: Reference equality / hashing?
class NaplParameter(naplType : NaplType, name : String)

case class NaplExpression[T](tag : T, expression : NaplExpression_[T]) extends NaplExpression_[T]

abstract class NaplExpression_[T]
case class LambdaExpression[T](params : Vector[NaplParameter], body : NaplExpression[T]) extends NaplExpression_[T]
case class ValueExpression[T](value : NaplValue) extends NaplExpression_[T]
case class ParameterExpression[T](param : NaplParameter) extends NaplExpression_[T]
case class OperatorExpression[T](opr : NaplOperator, args : Vector[NaplExpression[T]]) extends NaplExpression_[T]
case class ApplyExpression[T](funcExpr : NaplExpression[T], argExprs : Vector[NaplExpression[T]]) extends NaplExpression_[T]
case class InstantiateExpression[T](t : NaplType, exprs :  Vector[NaplExpression[T]]) extends NaplExpression_[T]
