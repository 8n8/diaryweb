module Main exposing (main)

import AccessCode exposing (AccessCode)
import Browser exposing (Document)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Row exposing (Row)
import Rows exposing (Rows)
import Task
import Time exposing (Month, Posix, Zone)


type Model
    = FatalError String
    | GettingTimeZone
    | Ok OkModel


type alias OkModel =
    { accessCodeBox : String
    , diaryEntryBox : String
    , rows : Rows
    , zone : Zone
    }


type Msg
    = AccessCode String
    | DiaryEntry String
    | TimeZone Zone


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
view model =
    { title = "Diary"
    , body =
        [ Element.layout
            [ Font.family [ Font.sansSerif ] ]
            (viewElement model)
        ]
    }


viewElement : Model -> Element Msg
viewElement model =
    case model of
        FatalError errorMessage ->
            viewFatalError errorMessage

        Ok okModel ->
            viewOk okModel

        GettingTimeZone ->
            Element.none


viewOk : OkModel -> Element Msg
viewOk { accessCodeBox, diaryEntryBox, rows, zone } =
    [ viewTopBar accessCodeBox
    , viewDiaryEntryBox diaryEntryBox
    , viewDiary zone rows
    ]
        |> Element.column []


viewDiary : Zone -> Rows -> Element msg
viewDiary zone rows =
    Rows.toList rows
        |> List.map (viewRow zone)
        |> Element.column [ Element.spacing 24 ]


viewRow : Zone -> Row -> Element msg
viewRow zone row =
    case row of
        Row.NotFound ->
            Element.none

        Row.Loaded time entry ->
            viewLoaded zone time entry

        Row.Loading ->
            Element.text "Loading..."


viewLoaded : Zone -> Posix -> String -> Element msg
viewLoaded zone time text =
    [ viewTimestamp zone time
    , viewDiaryEntryText text
    ]
        |> Element.paragraph []


viewDiaryEntryText : String -> Element msg
viewDiaryEntryText text =
    Element.text text


viewTimestamp : Zone -> Posix -> Element msg
viewTimestamp zone time =
    [ viewHourMinute (Time.toHour zone time) (Time.toMinute zone time)
    , " "
    , viewDayName (Time.toWeekday zone time)
    , " "
    , String.fromInt (Time.toDay zone time)
    , " "
    , viewMonth (Time.toMonth zone time)
    , " "
    , String.fromInt (Time.toYear zone time)
    ]
        |> String.concat
        |> Element.text


viewHourMinute : Int -> Int -> String
viewHourMinute hour minute =
    viewHour hour ++ ":" ++ viewMinute minute ++ " " ++ viewAmPm hour


viewHour : Int -> String
viewHour hour =
    String.fromInt (modBy 12 hour)


viewMinute : Int -> String
viewMinute minute =
    String.padLeft 2 '0' (String.fromInt minute)


viewAmPm : Int -> String
viewAmPm hour =
    if hour <= 12 then
        "am"

    else
        "pm"


viewDayName : Time.Weekday -> String
viewDayName weekday =
    case weekday of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


viewMonth : Month -> String
viewMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


viewDiaryEntryBox : String -> Element Msg
viewDiaryEntryBox diaryEntryBox =
    Input.multiline
        []
        { onChange = DiaryEntry
        , text = diaryEntryBox
        , placeholder = Nothing
        , label = Input.labelAbove [] (Element.text "Write a diary entry:")
        , spellcheck = True
        }


viewTopBar : String -> Element Msg
viewTopBar accessCodeBox =
    Input.text
        []
        { onChange = AccessCode
        , text = accessCodeBox
        , placeholder = Nothing
        , label = Input.labelLeft [] (Element.text "Access code:")
        }


viewFatalError : String -> Element msg
viewFatalError errorMessage =
    [ heading1 "Sorry, something went wrong"
    , paragraph "There was an internal error. This is as much as is known:"
    , monospace errorMessage
    ]
        |> Element.column [ Element.spacing 16 ]


heading1 : String -> Element msg
heading1 contents =
    Element.el [ Region.heading 1, Font.size 32 ] (Element.text contents)


paragraph : String -> Element msg
paragraph contents =
    Element.paragraph [ Font.size 16 ] [ Element.text contents ]


monospace : String -> Element msg
monospace contents =
    Element.text contents
        |> Element.el
            [ Font.size 16
            , Font.family [ Font.monospace ]
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( GettingTimeZone, Task.perform TimeZone Time.here )
