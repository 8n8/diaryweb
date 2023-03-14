module Rows exposing (Rows, empty, insert, toList)

import AccessCode exposing (AccessCode)
import Dict exposing (Dict)
import Row exposing (Row)


type Rows
    = Rows (Dict String Row) -- The String is the access code.


insert : AccessCode -> Row -> Rows -> Rows
insert accessCode row (Rows rows) =
    Rows (Dict.insert (AccessCode.toString accessCode) row rows)


empty : Rows
empty =
    Rows Dict.empty


toList : Rows -> List Row
toList (Rows rows) =
    Dict.values rows
