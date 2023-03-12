module Rows exposing (Rows, empty)

import Dict exposing (Dict)
import Row exposing (Row)


type Rows
    = Rows (Dict String Row) -- The String is the capability.


empty : Rows
empty =
    Rows Dict.empty
