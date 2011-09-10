module internal SoftMemes.Napl.Compilation.PlatformFunctions

open System
open System.Linq.Expressions

open SoftMemes.Napl
open SoftMemes.Napl.Runtime

let getRuntimeHelper =
    let helperT = typeof<RuntimeFunctions>
    fun methodName argts -> helperT.GetMethod(methodName, (argts : Type[]))

let initCollection typeName (t : Type) =
    // TODO: Cache
    let fullTypeName = sprintf "Microsoft.FSharp.Collections.%s" typeName
    let t = Type.GetType(fullTypeName, true)
    let mi = t.GetMethod("OfSeq")
    mi.MakeGenericMethod([|t|])

let rec getTupleItemNative (pe : ParameterExpression) i tupleT =
    // TODO: Cache members ...
    let propertyName = sprintf "Item%i" (i + 1)
    let propertyInfo = pe.Type.GetProperty(propertyName)
    Expression.Property(pe, propertyInfo)    

