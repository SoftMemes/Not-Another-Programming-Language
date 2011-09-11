module SoftMemes.Napl.Serialization.OperatorSerializer

open SoftMemes.Napl

let private operators = [    
    ConditionOperator, Serialization.NaplOperator.ConditionOperator;
    EqualsOperator, Serialization.NaplOperator.EqualsOperator;
    NotEqualsOperator, Serialization.NaplOperator.NotEqualsOperator;
    LessThanOperator, Serialization.NaplOperator.LessThanOperator;
    LessThanEqualsOperator, Serialization.NaplOperator.LessThanEqualsOperator;
    GreaterThanOperator, Serialization.NaplOperator.GreaterThanOperator;
    GreaterThanEqualsOperator, Serialization.NaplOperator.GreaterThanEqualsOperator;
    NotOperator, Serialization.NaplOperator.NotOperator;
    AndOperator, Serialization.NaplOperator.AndOperator;
    OrOperator, Serialization.NaplOperator.OrOperator;
    NegateOperator, Serialization.NaplOperator.NegateOperator;
    AddOperator, Serialization.NaplOperator.AddOperator;
    SubtractOperator, Serialization.NaplOperator.SubtractOperator;
    MultiplyOperator, Serialization.NaplOperator.MultiplyOperator;
    DivideOperator, Serialization.NaplOperator.DivideOperator;
    CurryOperator, Serialization.NaplOperator.CurryOperator;
    UncurryOperator, Serialization.NaplOperator.UncurryOperator;
    AppendOperator, Serialization.NaplOperator.AppendOperator;
    RemoveOperator, Serialization.NaplOperator.RemoveOperator;
    UnionOperator, Serialization.NaplOperator.UnionOperator;
    MinusOperator, Serialization.NaplOperator.MinusOperator;
    ZipOperator, Serialization.NaplOperator.ZipOperator;
    ContainsOperator, Serialization.NaplOperator.ContainsOperator;
    LookupOperator, Serialization.NaplOperator.LookupOperator;
    FoldOperator, Serialization.NaplOperator.FoldOperator ]

let private serializationMap = Map.ofList operators
let private deserializationMap = Map.ofList (operators |> List.map (fun (x,y) -> (y,x)))

let serialize opr =
    match Map.tryFind opr serializationMap with
    | Some(opr') -> opr'
    | None -> invalidArg "operator" (sprintf "Unsupported value: %A" opr)

let deserialize opr =
    match Map.tryFind opr deserializationMap with
    | Some(opr') -> opr'
    | None -> invalidArg "operator" (sprintf "Unsupported value: %A" opr)
