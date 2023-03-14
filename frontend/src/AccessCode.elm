module AccessCode exposing (AccessCode, decode)

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


type AccessCode
    = AccessCode Bytes


decode : Decoder AccessCode
decode =
    Decode.map AccessCode (Decode.bytes 16)
