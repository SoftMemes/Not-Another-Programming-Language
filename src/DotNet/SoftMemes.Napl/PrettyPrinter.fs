module SoftMemes.Napl.PrettyPrinter

open SoftMemes.Napl.Language

let PrintExpression (e : TaggedNaplExpression<_>) = sprintf "%A" e


