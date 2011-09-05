module SoftMemes.Napl.PrettyPrinter

open SoftMemes.Napl.Language

let PrintExpression (e : NaplExpression<_>) = sprintf "%A" e


