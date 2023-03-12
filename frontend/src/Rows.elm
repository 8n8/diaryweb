module Rows exposing (Rows)

import Row exposing (Row)
import Dict exposing (Dict)

type Rows
    = Rows (Dict String Row)
