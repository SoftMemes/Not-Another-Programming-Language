namespace SoftMemes.Napl.Linq

open System
open System.Collections.Generic
open System.Linq.Expressions
open SoftMemes.Napl

module Napl = SoftMemes.Napl.TaggedNaplExpressionBuilder

/// Functionally flavoured (although mutable) API for the .NET
/// Dictionary<,> type.
module internal HashMap =
    open System.Collections.Generic

    let add k v (dict : Dictionary<_,_>) = dict.Add(k, v)
    let remove k (dict : Dictionary<_,_>) = dict.Remove(k) |> ignore
    let find k (dict : Dictionary<_,_>) = dict.[k]
    let tryFind k (dict : Dictionary<_,_>) =
        match dict.TryGetValue(k) with
        | true, v -> Some v
        | false, _ -> None

module internal LinqConverter =
    let rec private typeToNapl =
        function
        | t when t = typeof<bool> -> BooleanType
        | t when t = typeof<string> -> StringType
        | t when t = typeof<int> -> IntegerType
        | t when t = typeof<float> -> FloatType
        | t when t.FullName.StartsWith("System.Func`") ->
            let args =
                t.GetGenericArguments()
                |> Seq.map typeToNapl
                |> List.ofSeq
            let tret = args |> List.rev |> List.head
            let ts = args |> List.rev |> List.tail |> List.rev
            FunctionType (ts, tret)

    let paramToNapl (param : ParameterExpression) =
        let t = typeToNapl param.Type
        {NaplParameter.Type = t; Name = param.Name}

    let valueToNapl (t : Type) (v : obj) =
        match t with
        | t when t = typeof<bool> -> BooleanValue (v :?> bool)
        | t when t = typeof<int> -> IntegerValue (v :?> int)
        | t when t = typeof<float> -> FloatValue (v :?> float)
        | t when t = typeof<string> -> StringValue (v :?> string)
        | _ ->
            let msg = sprintf "Constants of type %s are not supported by NAPL" t.Name
            raise (NotSupportedException(msg))

    let rec exprToNapl env (expr : Expression) =
        let notSupported expr =
            let msg = sprintf "The expression %O is not supported by NAPL" expr
            raise (NotSupportedException(msg))

        match expr with
        | :? LambdaExpression as lambdaExpr ->
            let naplParams = lambdaExpr.Parameters |> Seq.map paramToNapl |> Seq.toList
            Seq.zip lambdaExpr.Parameters naplParams
            |> Seq.iter (fun (p,naplP) -> HashMap.add p naplP env)
            let naplBody = exprToNapl env lambdaExpr.Body
            expr |> Napl.Lambda naplParams naplBody
        | :? ConditionalExpression as condExpr ->
            let naplTestE = exprToNapl env condExpr.Test
            let naplTrueE = exprToNapl env condExpr.IfTrue
            let naplFalseE = exprToNapl env condExpr.IfFalse
            expr |> Napl.If naplTestE naplTrueE naplFalseE
        | :? BinaryExpression as binaryExpr ->
            let opr =
                match binaryExpr.NodeType with
                | ExpressionType.Equal -> NaplOperator.EqualsOperator
                | ExpressionType.NotEqual -> NaplOperator.NotEqualsOperator
                | ExpressionType.LessThan -> NaplOperator.LessThanOperator
            let leftNaplE = exprToNapl env binaryExpr.Left
            let rightNaplE = exprToNapl env binaryExpr.Right
            expr |> Napl.BinaryOperator opr leftNaplE rightNaplE
        | :? ConstantExpression as constExpr ->
            let value = valueToNapl constExpr.Type constExpr.Value
            expr |> Napl.Value value
        | :? ParameterExpression as paramExpr ->
            let naplParam = HashMap.find paramExpr env
            expr |> Napl.Parameter naplParam
        | expr -> notSupported expr

open System.Runtime.CompilerServices

[<Extension>]
module LinqExtensions =
    [<Extension>]   
    let ToNapl(expr : Expression) = LinqConverter.exprToNapl (Dictionary()) expr
