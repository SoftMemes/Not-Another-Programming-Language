namespace SoftMemes.Napl.Compilation

open System
open System.Runtime.Serialization

[<Serializable>]
type NaplValidationException =
    inherit System.Exception
    new() = { inherit System.Exception() }
    new(message) = { inherit System.Exception(message) }
    new(message, innerException) = {
        inherit System.Exception((message : string), innerException) }
    new(serializationInfo, streamingContext) = {
        inherit System.Exception(
            (serializationInfo : SerializationInfo),
            streamingContext) }

module internal ErrorReporter =
    let referenceError expr message =
        // TODO: Pretty print expression, showing only limited depth.
        let message = sprintf "Reference error in %A: %s" expr message
        raise <| NaplValidationException(message)

    let typeError expr expected ts =
        // TODO: Pretty print expression, showing only limited depth.
        let message = sprintf "Type error in %A: Expected %s, but found %A" expr expected ts
        raise <| NaplValidationException(message)
