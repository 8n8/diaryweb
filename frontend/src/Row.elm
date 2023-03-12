module Row exposing (Row)

import Capability exposing (Capability)


type Row
    = NotFound
    | NotRequested Capability
    | Loaded String
    | Loading
