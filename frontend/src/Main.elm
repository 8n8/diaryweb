module Main exposing (main)

import Html
import Rows exposing (Rows)
import Capability exposing (Capability)

type Model
    = FatalError String
    | EnterAccessCode String
    | EnterDiary String
    | ViewDiary Diary


type alias Diary =
    { capability : Capability
    , rows : Rows
    }


main =
    Html.text "Hi"
