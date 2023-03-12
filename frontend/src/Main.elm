module Main exposing (main)

import Browser exposing (Document)
import Capability exposing (Capability)
import Html
import Rows exposing (Rows)


type Model
    = FatalError String
    | Ok OkModel


type alias OkModel =
    { accessCodeBox : String
    , diaryEntryBox : String
    , rows : Rows
    }


type Msg
    = Msg


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Document Msg
view _ =
    { title = "Diary"
    , body = [ Html.text "Hello" ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { accessCodeBox = ""
      , diaryEntryBox = ""
      , rows = Rows.empty
      }
        |> Ok
    , Cmd.none
    )
