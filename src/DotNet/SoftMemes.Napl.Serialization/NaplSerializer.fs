module SoftMemes.Napl.NaplSerializer

open SoftMemes.Napl
open SoftMemes.Napl.Serialization

let Serialize e = ExpressionSerializer.serialize (Map.empty, 0) e
