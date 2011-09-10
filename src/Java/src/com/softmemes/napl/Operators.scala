package com.softmemes.napl

abstract class NaplOperator
// Control flow
case class ConditionOperator extends NaplOperator
// Equality
case class EqualsOperator extends NaplOperator
case class NotEqualsOperator extends NaplOperator
// Ordering
case class LessThanOperator extends NaplOperator
case class LessThanEqualsOperator extends NaplOperator
case class GreaterThanOperator extends NaplOperator
case class GreaterThanEqualsOperator extends NaplOperator
// Logic
case class NotOperator extends NaplOperator
case class AndOperator extends NaplOperator
case class OrOperator extends NaplOperator
// Numeric
case class NegateOperator extends NaplOperator
case class AddOperator extends NaplOperator
case class SubtractOperator extends NaplOperator
case class MultiplyOperator extends NaplOperator
case class DivideOperator extends NaplOperator
// Tuples
case class CurryOperator extends NaplOperator
case class UncurryOperator extends NaplOperator
// Collections
case class AppendOperator extends NaplOperator
case class RemoveOperator extends NaplOperator
case class UnionOperator extends NaplOperator
case class MinusOperator extends NaplOperator
case class ZipOperator extends NaplOperator
case class ContainsOperator extends NaplOperator
case class LookupOperator extends NaplOperator
case class FoldOperator extends NaplOperator

object Operators {
  private abstract class NaplOperatorFamily
  private case class ControlFlowOperatorFamily extends NaplOperatorFamily
  private case class EqualityOperatorFamily extends NaplOperatorFamily
  private case class OrderingOperatorFamily extends NaplOperatorFamily
  private case class LogicOperatorFamily extends NaplOperatorFamily
  private case class NumericOperatorFamily extends NaplOperatorFamily
  private case class TupleOperatorFamily extends NaplOperatorFamily
  private case class CollectionOperatorFamily extends NaplOperatorFamily

  private abstract class NaplOperatorArity
  private case class UnaryOperatorArity extends NaplOperatorArity
  private case class BinaryOperatorArity extends NaplOperatorArity
  private case class TrinaryOperatorArity extends NaplOperatorArity
  
  private def categorizeOperator(opr : NaplOperator) : (NaplOperatorFamily, NaplOperatorArity) = opr match {
    case ConditionOperator()         => (ControlFlowOperatorFamily(), TrinaryOperatorArity())
    // Equality
    case EqualsOperator()
       | NotEqualsOperator()         => (EqualityOperatorFamily(), BinaryOperatorArity())
    // Ordering
    case LessThanOperator()
       | LessThanEqualsOperator()
       | GreaterThanOperator()
       | GreaterThanEqualsOperator() => (OrderingOperatorFamily(), BinaryOperatorArity())
    // Logic
    case NotOperator()               => (LogicOperatorFamily(), UnaryOperatorArity())
    case AndOperator()
       | OrOperator()                => (LogicOperatorFamily(), BinaryOperatorArity())
    // Numeric
    case NegateOperator()            => (NumericOperatorFamily(), UnaryOperatorArity())
    case AddOperator()
       | SubtractOperator()
       | MultiplyOperator()
       | DivideOperator()            => (NumericOperatorFamily(), BinaryOperatorArity())
    // Tuples
    case CurryOperator()
       | UncurryOperator()           => (TupleOperatorFamily(), UnaryOperatorArity())
    // Collections
    case AppendOperator()
       | RemoveOperator() 
       | UnionOperator()
       | MinusOperator()
       | ZipOperator()
       | ContainsOperator()
       | LookupOperator()            => (CollectionOperatorFamily(), BinaryOperatorArity())
    case FoldOperator()              => (CollectionOperatorFamily(), TrinaryOperatorArity())
  }
//
//    let (|ControlFlowOperator|EqualityOperator|OrderingOperator|LogicOperator|NumericOperator|TupleOperator|CollectionOperator|) opr =
//        match categorizeOperator opr |> fst with
//        | ControlFlowOperatorFamily => ControlFlowOperator
//        | EqualityOperatorFamily => EqualityOperator
//        | OrderingOperatorFamily => OrderingOperator
//        | LogicOperatorFamily => LogicOperator
//        | NumericOperatorFamily => NumericOperator
//        | TupleOperatorFamily => TupleOperator
//        | CollectionOperatorFamily => CollectionOperator
//
//    let (|UnaryOperator|BinaryOperator|TrinaryOperator|NAryOperator|) opr =
//        match categorizeOperator opr |> snd with
//        | UnaryOperatorArity => UnaryOperator
//        | BinaryOperatorArity => BinaryOperator
//        | TrinaryOperatorArity => TrinaryOperator
//        | NAryOperatorArity => NAryOperator
}