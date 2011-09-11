module SoftMemes.Napl.Compilation.OperatorCompiler

open System
open System.Linq.Expressions

open SoftMemes.Napl
open SoftMemes.Napl.Operators
open SoftMemes.Napl.Types
open SoftMemes.Napl.Compilation.ErrorReporter
open SoftMemes.Napl.Compilation.PlatformFunctions
open SoftMemes.Napl.Compilation.TypeCompiler

type LinqExpression = System.Linq.Expressions.Expression

let typeCheck expr opr ts =
    match opr with
    | ConditionOperator ->
        match ts with
        | [BooleanType;truet;falset] when truet = falset -> truet
        | _ -> typeError expr "[bool,<T1>,<T2>] where t1 = t2" ts
    | EqualityOperator ->
        match ts with
        | [t1;t2] when t1 = t2 -> t1
        | _ -> typeError expr "[<T1>,<T2>] where t1 = t2" ts
    | OrderingOperator ->
        match ts with
        | [NumericType as t1; NumericType as t2] when t1 = t2 -> BooleanType
        | _ -> typeError expr "[<T1>,<T2>] where t1 is numeric, t1 = t2" ts
    | LogicOperator ->
        match ts with
        | [BooleanType; BooleanType] -> BooleanType
        | _ -> typeError expr "[bool,bool]" ts
    | NumericOperator ->
        match opr, ts with
        | UnaryOperator, [NumericType as t] -> t
        | UnaryOperator, _ -> typeError expr "[<T>] where T is numeric" ts
        | BinaryOperator, [NumericType as t1; NumericType as t2] when t1 = t2 -> t1
        | BinaryOperator, _ -> typeError expr "[<T1>,<T2>] where T1 is numeric, t1 = t2" ts
    | TupleOperator -> TupleType ts
    | CurryOperator ->
        match ts with
        | [FunctionType ([TupleType ts],tret)] -> FunctionType (ts, tret)
        | _ -> typeError expr "<T1> -> <T2> where T1 is tuple" ts
    | UncurryOperator ->
        match ts with
        | [FunctionType (ts, tret)] -> FunctionType ([TupleType ts], tret)
        | _ -> typeError expr "function" ts
    | AppendOperator
    | RemoveOperator ->
        match ts with 
        | [t; CollectionType t'] when t = t' -> t
        | _ -> typeError expr "[<T1>,collection of <T1>]" ts
    | UnionOperator
    | MinusOperator ->
        match ts with
        | [(CollectionType t1) as tc; CollectionType t2] when t1 = t2 -> tc
        | _ -> typeError expr "[collection of <T1>,collection of <T1>]" ts
    | ZipOperator ->
        match ts with
        | [CollectionType t1; CollectionType t2] ->
            TupleType [t1;t2] |> ListType
        | _ -> typeError expr "[collection of of <T1>,collection of <T2>]" ts
    | ContainsOperator ->
        match ts with 
        | [t; ListType t']
        | [t; SetType t']
        | [t; MapType (t',_)] -> BooleanType
        | _ -> typeError expr "[<T>,collection of <T>]" ts
    | LookupOperator ->
        match ts with 
        | [t; ListType t'] when t = IntegerType -> t'
        | [_; ListType _] -> typeError expr "[integer,list of <T>]" ts
        | [t; SetType t'] when t = t' -> BooleanType
        | [_; SetType _] -> typeError expr "[<T>,set of <T>]" ts
        | [t; MapType (tk, tv)] when t = tk -> tv
        | [_; MapType _] -> typeError expr "[<T1>,map of <T1>,<T2>]" ts
        | _ -> typeError expr "[<T1>,<T2>]" ts
    | FoldOperator ->
        match ts with
        | [FunctionType ([at], t);at';CollectionType t'] when at = at' && t = t' -> at
        | _ -> typeError expr "[(<T1>, <T2>) -> <T1>,<T1>,collection of <T2>]" ts

let compile compileExpression t opr exprs : LinqExpression =
    let exprTs = exprs |> List.map getType
    let nativeTs = exprTs |> List.map getNativeType
    match opr, exprs with
    | CurryOperator, [fExpr] ->
        let (FunctionType (ts, tret)) = t
        let nativeTret = getNativeType tret
        let nativeParam = getNativeType (TupleType ts)
        let nativeInT = createFuncType [nativeParam] nativeTret
        let nativeCurry = getRuntimeHelper "Curry" [|nativeInT|]
        let nativeF = compileExpression fExpr : Expression
        upcast LinqExpression.Call(nativeCurry, nativeF)
    | UncurryOperator, [fExpr] ->
        let (FunctionType ([TupleType ts], tret)) = t
        let nativeTret = getNativeType tret
        let nativeParams = ts |> List.map getNativeType
        let nativeInT = createFuncType nativeParams nativeTret
        let nativeUncurry = getRuntimeHelper "Uncurry" [|nativeInT|]
        let nativeF = compileExpression fExpr
        upcast LinqExpression.Call(nativeUncurry, nativeF)
    | BinaryOperator, [leftExpr;rightExpr] ->
        let nativeLeftExpr = compileExpression leftExpr
        let nativeRightExpr = compileExpression rightExpr
        match opr with
        | EqualsOperator ->
            upcast LinqExpression.Equal(nativeLeftExpr, nativeRightExpr)
        | NotEqualsOperator ->
            upcast LinqExpression.NotEqual(nativeLeftExpr, nativeRightExpr)
        | LessThanOperator ->
            upcast LinqExpression.LessThan(nativeLeftExpr, nativeRightExpr)
        | LessThanEqualsOperator ->
            upcast LinqExpression.LessThanOrEqual(nativeLeftExpr, nativeRightExpr)
        | GreaterThanOperator ->
            upcast LinqExpression.GreaterThan(nativeLeftExpr, nativeRightExpr)
        | GreaterThanEqualsOperator ->
            upcast LinqExpression.GreaterThanOrEqual(nativeLeftExpr, nativeRightExpr)
        | AndOperator ->
            upcast LinqExpression.And(nativeLeftExpr, nativeRightExpr)
        | OrOperator ->
            upcast LinqExpression.Or(nativeLeftExpr, nativeRightExpr)
        | AddOperator ->
            upcast LinqExpression.Add(nativeLeftExpr, nativeRightExpr)
        | SubtractOperator ->
            upcast LinqExpression.Subtract(nativeLeftExpr, nativeRightExpr)
        | MultiplyOperator ->
            upcast LinqExpression.Multiply(nativeLeftExpr, nativeRightExpr)
        | DivideOperator ->
            upcast LinqExpression.Divide(nativeLeftExpr, nativeRightExpr)
    | ConditionOperator, [condExpr;trueExpr;falseExpr] ->
        let nativeCondExpr = compileExpression condExpr
        let nativeTrueExpr = compileExpression trueExpr
        let nativeFalseExpr = compileExpression falseExpr
        let nativeT = getNativeType t
        upcast LinqExpression.Condition(
            nativeCondExpr, nativeTrueExpr, nativeFalseExpr, nativeT)
// TODO: Implement other operators ...
