module AccessCode exposing (AccessCode, decode, toBytes, toString)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


type AccessCode
    = AccessCode Bytes


decode : Decoder AccessCode
decode =
    Decode.map AccessCode (Decode.bytes 16)


toString : AccessCode -> String
toString (AccessCode bytes) =
    Base64.fromBytes bytes |> Maybe.withDefault ""


toBytes : AccessCode -> Bytes
toBytes (AccessCode bytes) =
    bytes
