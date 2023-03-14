module Row exposing (Row(..))

import AccessCode exposing (AccessCode)
import Time exposing (Posix)


type Row
    = NotFound
    | Loaded Posix String
    | Loading
