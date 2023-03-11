module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Diary


suite : Test
suite =
    describe "Diary"
        [ test "initial view" <| \_ -> testInitialView
        ]


testInitialView =
    let
        (model, _) = Diary.init Route.NewDiary
        html = Diary.view model
        expected = 
            [ Input.multiline
                []
                { onChange = 
    in

    
