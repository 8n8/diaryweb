module UserCode exposing (UserCode, fromString, toBytes)


import Bytes exposing (Bytes)
import Base64


type UserCode
    = UserCode Bytes


toBytes : UserCode -> Bytes
toBytes (UserCode bytes) =
    bytes


fromString : String -> Maybe UserCode
fromString raw =
    case Base64.toBytes raw of
        Nothing ->
            Nothing

        Just bytes ->
            if Bytes.width bytes == 16 then
                Just (UserCode bytes)

            else
                Nothing
