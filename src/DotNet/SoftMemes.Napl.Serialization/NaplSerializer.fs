module SoftMemes.Napl.NaplSerializer

open SoftMemes.Napl
open SoftMemes.Napl.Serialization

let SerializeExpression e = ExpressionSerializer.serialize (Map.empty, 0) e
let SerializeType t = TypeSerializer.serialize t

let DeserializeExpression e = ExpressionSerializer.deserialize Map.empty e
let DeserializeType t = TypeSerializer.deserialize t
