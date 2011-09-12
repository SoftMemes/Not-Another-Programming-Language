namespace SoftMemes.Napl

open System
open System.Runtime.Serialization

[<Serializable>]
type NaplSerializationException =
    inherit System.Exception
    new() = { inherit System.Exception() }
    new(message) = { inherit System.Exception(message) }
    new(message, innerException) = {
        inherit System.Exception((message : string), innerException) }
    new(serializationInfo, streamingContext) = {
        inherit System.Exception(
            (serializationInfo : SerializationInfo),
            streamingContext) }

namespace SoftMemes.Napl.Serialization
open SoftMemes.Napl

module internal ErrorReporter =
    let serializationError message =
        raise <| NaplSerializationException(message)
