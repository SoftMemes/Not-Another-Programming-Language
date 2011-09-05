namespace SoftMemes.Napl

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


