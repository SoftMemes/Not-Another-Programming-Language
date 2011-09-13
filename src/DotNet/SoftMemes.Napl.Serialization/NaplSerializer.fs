module SoftMemes.Napl.NaplSerializer

open SoftMemes.Napl
open SoftMemes.Napl.Serialization

let SerializeType t = TypeSerializer.serialize t
let SerializeExpression e = ExpressionSerializer.serialize (Map.empty, 0) e
