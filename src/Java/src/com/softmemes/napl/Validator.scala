package com.softmemes.napl

import scala.collection.immutable.Set
import scala.collection.immutable.TreeSet

object Validator {

  private def referenceError (str : String) = {
    throw new RuntimeException(str)
  }
  
  private def typeError[T](expr : NaplExpression[T], expected : String, ts : Seq[NaplType]) = {
    // TODO: Pretty print expression, showing only limited depth.
    val message = "Type error in %s: Expected %s, but found %s".format(expr, expected, ts)
    throw new RuntimeException(message)
  }
  
  private def invalidArg(message : String) =
    throw new IllegalArgumentException(message)

  private def collectionCastType(cType : NaplCollectionType, t_ : NaplType) =
    (cType, t_) match {
      case (ListType(_), _) => ListType(t_)
      case (SetType(_), _) => SetType(t_)
      case (MapType(_,_), TupleType(Seq(tk,tv))) => MapType (tk, tv)
      case (MapType(_,_), t) => invalidArg("Type argument must be a tuple for maps")
      case _ => invalidArg("Argument is not a collection type") }

  private def referenceCheck[T](env : Set[NaplParameter], expr : NaplExpression[T]) : Unit =
    expr.expression match {
      case ParameterExpression(p) =>
  		if (!env.contains(p))
          referenceError ("Parameter %s is not in scope".format(p))
      case LambdaExpression(ps, expr) =>
        val env_ = env ++ ps
        ExpressionVisitor.visit (referenceCheck[T], env_, expr)
      case _ =>
        ExpressionVisitor.visit (referenceCheck[T], env, expr)
	}

  private def typeCheckOperator[T] (
      expr : NaplExpression[T],
      opr : NaplOperator,
      ts : IndexedSeq[NaplType]) = {
    opr match {
      case ConditionOperator() =>
        ts match {
          case Seq(BooleanType(),truet,falset) =>
            if (truet == falset) truet
            else typeError(expr, "[bool,<T1>,<T2>] where t1 = t2", ts)           
          case _ => typeError(expr, "[bool,<T1>,<T2>] where t1 = t2", ts)
        }
      case EqualityOperator() =>
        case Seq(t1,t2) =>
          if (t1 == t2) t1
          else typeError(expr, "[<T1>,<T2>] where t1 = t2", ts)
        case _ => typeError(expr, "[<T1>,<T2>] where t1 = t2", ts)
    }    
  }
//let private typeCheckOperator expr opr ts =
//    match opr with
//    | ConditionOperator ->
//        match ts with
//        | [BooleanType;truet;falset] when truet = falset -> truet
//        | _ -> typeError expr "[bool,<T1>,<T2>] where t1 = t2" ts
//    | EqualityOperator ->
//        match ts with
//        | [t1;t2] when t1 = t2 -> t1
//        | _ -> typeError expr "[<T1>,<T2>] where t1 = t2" ts
//    | OrderingOperator ->
//        match ts with
//        | [NumericType as t1; NumericType as t2] when t1 = t2 -> BooleanType
//        | _ -> typeError expr "[<T1>,<T2>] where t1 is numeric, t1 = t2" ts
//    | LogicOperator ->
//        match ts with
//        | [BooleanType; BooleanType] -> BooleanType
//        | _ -> typeError expr "[bool,bool]" ts
//    | NumericOperator ->
//        match opr, ts with
//        | UnaryOperator, [NumericType as t] -> t
//        | UnaryOperator, _ -> typeError expr "[<T>] where T is numeric" ts
//        | BinaryOperator, [NumericType as t1; NumericType as t2] when t1 = t2 -> t1
//        | BinaryOperator, _ -> typeError expr "[<T1>,<T2>] where T1 is numeric, t1 = t2" ts
//    | TupleOperator -> TupleType ts
//    | CurryOperator ->
//        match ts with
//        | [FunctionType ([TupleType ts],tret)] -> FunctionType (ts, tret)
//        | _ -> typeError expr "<T1> -> <T2> where T1 is tuple" ts
//    | UncurryOperator ->
//        match ts with
//        | [FunctionType (ts, tret)] -> FunctionType ([TupleType ts], tret)
//        | _ -> typeError expr "function" ts
//    | AppendOperator
//    | RemoveOperator ->
//        match ts with 
//        | [t; CollectionOfType t'] when t = t' -> t
//        | _ -> typeError expr "[<T1>,collection of <T1>]" ts
//    | UnionOperator
//    | MinusOperator ->
//        match ts with
//        | [(CollectionOfType t1) as tc; CollectionOfType t2] when t1 = t2 -> tc
//        | _ -> typeError expr "[collection of <T1>,collection of <T1>]" ts
//    | ZipOperator ->
//        match ts with
//        | [CollectionOfType t1; CollectionOfType t2] ->
//            TupleType [t1;t2] |> ListType |> CollectionType
//        | _ -> typeError expr "[collection of of <T1>,collection of <T2>]" ts
//    | ContainsOperator ->
//        match ts with 
//        | [t; ListOfType t']
//        | [t; SetOfType t']
//        | [t; MapOfKeyType t'] -> BooleanType
//        | _ -> typeError expr "[<T>,collection of <T>]" ts
//    | LookupOperator ->
//        match ts with 
//        | [t; ListOfType t'] when t = IntegerType -> t'
//        | [_; ListOfType _] -> typeError expr "[integer,list of <T>]" ts
//        | [t; SetOfType t'] when t = t' -> BooleanType
//        | [_; SetOfType _] -> typeError expr "[<T>,set of <T>]" ts
//        | [t; MapOfType (tk, tv)] when t = tk -> tv
//        | [_; MapOfType _] -> typeError expr "[<T1>,map of <T1>,<T2>]" ts
//        | _ -> typeError expr "[<T1>,<T2>]" ts
//    | FoldOperator ->
//        match ts with
//        | [FunctionType ([at], t);at';CollectionOfType t'] when at = at' && t = t' -> at
//        | _ -> typeError expr "[(<T1>, <T2>) -> <T1>,<T1>,collection of <T2>]" ts
//
//let rec private typeCheck (NaplExpression (tag, expr) as expr') =
//    let ret t' expr = NaplExpression ((t',tag), expr)
//    let extract (NaplExpression ((exprT, _), _) as expr) = exprT, expr
//    let typeCheck' = typeCheck >> extract
//    match expr with
//    | LambdaExpression (param, expr) ->
//        let exprT, expr' = typeCheck' expr
//        ret exprT (LambdaExpression (param, expr'))
//    | ValueExpression value ->
//        match value with
//        | BooleanValue _ -> ret BooleanType (ValueExpression value)
//        | IntegerValue _ -> ret IntegerType (ValueExpression value)
//        | FloatValue _ -> ret FloatType (ValueExpression value)
//        | StringValue _ -> ret StringType (ValueExpression value)
//    | ParameterExpression (NaplParameter (t, _) as p) ->
//        ret t (ParameterExpression p)
//    | OperatorExpression (opr, exprs) ->
//        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
//        let t = typeCheckOperator expr opr ts
//        ret t (OperatorExpression (opr, exprs'))
//    | CollectionExpression (t, exprs) ->
//        let ts, exprs' = exprs |> List.map typeCheck' |> List.unzip
//        match t with
//        | CollectionTypeOf t' when List.forall (fun t'' -> t' = t'') ts ->
//            ret (CollectionType t) (CollectionExpression (t, exprs'))
//        | CollectionTypeOf t' ->
//            typeError expr (sprintf "collection of %A" t') ts
//    | ApplyExpression (funcExpr, paramExprs) ->
//        let funcT, funcExpr' = typeCheck' funcExpr
//        let paramTs, paramExprs' = paramExprs |> List.map typeCheck' |> List.unzip
//        match funcT with
//        | FunctionType (pts, t) when pts = paramTs ->
//            ret t (ApplyExpression (funcExpr', paramExprs'))
//        | FunctionType (pts, t) ->
//            typeError expr (sprintf "%A" pts) paramTs
//        | t ->
//            typeError expr "function type" paramTs
//
//let TypeCheck e =
//    referenceCheck Set.empty e
//    typeCheck e
}