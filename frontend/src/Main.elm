module Main exposing (main)

import AccessCode exposing (AccessCode)
import Base64
import Browser exposing (Document)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode
import Element exposing (Element)
import UserCode exposing (UserCode)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Http
import Indicator
import Row exposing (Row)
import Rows exposing (Rows)
import Task
import Table exposing (Table)
import Time exposing (Month, Posix, Zone)


type Model
    = FatalError String
    | GettingTimeZone
    | Ok OkModel
    | NoInternet


type alias OkModel =
    { accessCodeBox : String
    , diaryEntryBox : String
    , table : Maybe Table
    , zone : Zone
    }


type Msg
    = AccessCode String
    | DiaryEntry String
    | TimeZone Zone
    | SubmitDiaryEntry
    | SubmitAccessCode UserCode
    | GotTable (Result Http.Error (List AccessCode))
    | RowResponse AccessCode (Result Http.Error ( Posix, String ))


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        FatalError _ ->
            ( model, Cmd.none )

        GettingTimeZone ->
            updateGettingTimeZone msg

        Ok okModel ->
            updateOk msg okModel

        NoInternet ->
            ( model, Cmd.none )


updateGettingTimeZone : Msg -> ( Model, Cmd Msg )
updateGettingTimeZone msg =
    case msg of
        AccessCode _ ->
            ( GettingTimeZone, Cmd.none )

        DiaryEntry _ ->
            ( GettingTimeZone, Cmd.none )

        TimeZone zone ->
            ( { accessCodeBox = ""
              , diaryEntryBox = ""
              , table = Nothing
              , zone = zone
              }
                |> Ok
            , Cmd.none
            )

        SubmitAccessCode _ ->
            ( GettingTimeZone, Cmd.none )

        GotTable _ ->
            ( GettingTimeZone, Cmd.none )

        RowResponse _ _ ->
            ( GettingTimeZone, Cmd.none )


updateOk : Msg -> OkModel -> ( Model, Cmd Msg )
updateOk msg model =
    case msg of
        AccessCode accessCode ->
                    ( Ok { model | accessCodeBox = accessCode }, Cmd.none )

        DiaryEntry diaryEntry ->
            ( Ok { model | diaryEntryBox = diaryEntry }, Cmd.none )

        TimeZone zone ->
            ( Ok { model | zone = zone }, Cmd.none )

        SubmitAccessCode accessCode ->
            ( Ok { model | rows = Loading }
            , { url = "/api"
              , body =
                    Http.bytesBody
                        "application/octet-stream"
                        ([ Encode.unsignedInt8 Indicator.get
                         , Encode.bytes (UserCode.toBytes accessCode)
                         ]
                            |> Encode.sequence
                            |> Encode.encode
                        )
              , expect = Http.expectBytes GotTable decodeGotTable
              }
                |> Http.post
            )

        SubmitDiaryEntry ->
            case model of
                FatalError _ -> 
                    ( Ok model, Cmd.none)

                GettingTimeZone ->
                    (Ok model, Cmd.none)

                NoInternet ->
                    (Ok model, Cmd.none )


        GotTable (Err (Http.BadUrl error)) ->
            ( FatalError ("bad URL when fetching table: " ++ error)
            , Cmd.none
            )

        GotTable (Err Http.Timeout) ->
            ( NoInternet, Cmd.none )

        GotTable (Err Http.NetworkError) ->
            ( NoInternet, Cmd.none )

        GotTable (Err (Http.BadStatus status)) ->
            ( [ "bad status when fetching table: "
              , String.fromInt status
              ]
                |> String.concat
                |> FatalError
            , Cmd.none
            )

        GotTable (Err (Http.BadBody badBody)) ->
            ( [ "bad body when fetching table: "
              , badBody
              ]
                |> String.concat
                |> FatalError
            , Cmd.none
            )

        GotTable (Result.Ok accessCodes) ->
            ( Ok { model | rows = Loaded (makeRows accessCodes) }
            , makeRowRequests accessCodes)
        RowResponse _ (Err (Http.BadUrl error)) ->
            ( FatalError ("bad URL when fetching row: " ++ error)
            , Cmd.none)
        RowResponse _ (Err Http.Timeout) ->
            ( NoInternet, Cmd.none )
        RowResponse _ (Err Http.NetworkError) ->
            ( NoInternet, Cmd.none )
        RowResponse _ (Err (Http.BadStatus status)) ->
            ( [ "bad status when fetching row: ", String.fromInt status ]
                |> String.concat |> FatalError
            , Cmd.none)
        RowResponse _ (Err (Http.BadBody badBody)) ->
            ( [ "bad body when fetching row: ", badBody ]
                |> String.concat |> FatalError
            , Cmd.none)
        RowResponse accessCode (Result.Ok (posix, diaryEntry)) ->
            ( { model
                | table =
                    Table.updateRow
                        accessCode
                        (Row.Loaded posix diaryEntry)
                        model.table
              }
            , Cmd.none
            )


makeRows : List AccessCode -> Rows
makeRows accessCodes =
    List.foldr makeRowsHelp Rows.empty accessCodes


makeRowsHelp : AccessCode -> Rows -> Rows
makeRowsHelp accessCode rows =
    Rows.insert accessCode Row.Loading rows


makeRowRequests : List AccessCode -> Cmd Msg
makeRowRequests accessCodes =
    List.map makeRowRequest accessCodes |> Cmd.batch


makeRowRequest : AccessCode -> Cmd Msg
makeRowRequest accessCode =
    { url = "/api"
    , body =
        Http.bytesBody "application/octet-stream" (getRowBody accessCode)
    , expect = Http.expectBytes (RowResponse accessCode) decodeRow
    }
        |> Http.post


getRowBody : AccessCode -> Bytes
getRowBody accessCode =
    [ Encode.unsignedInt8 Indicator.get
    , Encode.bytes (AccessCode.toBytes accessCode)
    ]
        |> Encode.sequence
        |> Encode.encode


decodeRow : Decoder ( Posix, String )
decodeRow =
    decodeRaw |> Decode.andThen decodeRowHelp


decodeRowHelp : Bytes -> Decoder ( Posix, String )
decodeRowHelp bytes =
    case Decode.decode decodeDiaryEntry bytes of
        Nothing ->
            Decode.fail

        Just diaryEntry ->
            Decode.succeed diaryEntry


decodeDiaryEntry : Decoder ( Posix, String )
decodeDiaryEntry =
    Decode.map2 (\posix entry -> ( posix, entry ))
        decodePosix
        (Decode.unsignedInt16 Bytes.LE |> Decode.andThen Decode.string)


decodePosix : Decoder Posix
decodePosix =
    Decode.unsignedInt32 Bytes.LE
        |> Decode.map
            (\raw ->
                Time.millisToPosix (raw * 1000)
            )


decodeRaw : Decoder Bytes
decodeRaw =
    Decode.unsignedInt32 Bytes.LE
        |> Decode.andThen
            (\numRows ->
                if numRows == 1 then
                    Decode.unsignedInt16 Bytes.LE |> Decode.andThen Decode.bytes

                else
                    Decode.fail
            )


decodeGotTable : Decoder (List AccessCode)
decodeGotTable =
    Decode.unsignedInt32 Bytes.LE
        |> Decode.andThen
            (\len ->
                Decode.loop ( len, [] ) decodeGotTableHelp
            )


decodeGotTableHelp :
    ( Int, List AccessCode )
    -> Decoder (Decode.Step ( Int, List AccessCode ) (List AccessCode))
decodeGotTableHelp ( n, xs ) =
    if n <= 0 then
        Decode.succeed (Decode.Done xs)

    else
        Decode.map (\x -> Decode.Loop ( n - 1, x :: xs )) AccessCode.decode


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

        NoInternet ->
            Element.text "no internet connection"


viewOk : OkModel -> Element Msg
viewOk { accessCodeBox, diaryEntryBox, rows, zone } =
    [ viewTopBar accessCodeBox
    , viewDiaryEntryBox diaryEntryBox
    , case rows of
        Loaded loaded ->
            viewDiary zone loaded

        Loading ->
            Element.text "Loading..."

        NoSuchTable ->
            Element.text "Not found"

        NotAsked ->
            Element.none
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

        Row.Corrupted ->
            Element.text "Corrupted data"


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
    [ Input.multiline
        []
        { onChange = DiaryEntry
        , text = diaryEntryBox
        , placeholder = Nothing
        , label = Input.labelAbove [] (Element.text "Write a diary entry:")
        , spellcheck = True
        }
    , Input.button
        []
        { onPress = Just SubmitDiaryEntry
        , label = Element.text "Submit"
        }
    ]
        |> Element.column []


viewTopBar : String -> Element Msg
viewTopBar accessCodeBox =
    [ Input.text
        []
        { onChange = AccessCode
        , text = accessCodeBox
        , placeholder = Nothing
        , label = Input.labelLeft [] (Element.text "Access code:")
        }
    ,       case UserCode.fromString accessCodeBox of
                Nothing ->
                    accessCodeHelp

                Just valid ->
                        submitAccessCode valid
    ]
        |> Element.row []


accessCodeHelp =
    Element.text "The access code should be around 20 mixed characters."


submitAccessCode accessCodeBox =
    Input.button
        []
        { onPress = Just (SubmitAccessCode accessCodeBox)
        , label = Element.text "Submit"
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
