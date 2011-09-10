package com.softmemes.napl

abstract class NaplType
case class BooleanType extends NaplType
case class StringType extends NaplType
case class IntegerType extends NaplType
case class FloatType extends NaplType
case class TupleType(itemTypes : Vector[NaplType]) extends NaplType
case class FunctionType(funcType : Vector[NaplType], retType : NaplType) extends NaplType
case class ListType(itemType : NaplType) extends NaplType
case class SetType(itemType : NaplType) extends NaplType
case class MapType(keyType : NaplType, valueType : NaplType) extends NaplType

object Types {
  object NumericType {
    def unapply (t : NaplType) = t match {
      case IntegerType() | FloatType() => true
      case _ => false
    }
  }
  
  object CollectionType {
    def unapply (t : NaplType) = t match {
      case ListType(t) => Some(t)
      case SetType(t) => Some(t)
      case MapType(tk,tv) => Some(TupleType(Vector(tk,tv)))
      case _ => None
    }
  }  
}