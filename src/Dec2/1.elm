module Main exposing (Model, init, main, update, view)

import Browser
import Dec2.InputData
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String }


init : Model
init =
    { input = Dec2.InputData.data }


update msg model =
    model



-- VIEW


hasSame : Char -> Int -> String -> Bool
hasSame char amount str =
    str
        |> String.filter (\c -> c == char)
        |> String.length
        |> (==) amount


containsSame : Int -> String -> Bool
containsSame amount str =
    String.any (\char -> hasSame char amount str) str


calculateResult : String -> String
calculateResult input =
    let
        inputList =
            input
                |> String.split "\n"
                |> List.map String.trim

        withTwo =
            inputList
                |> List.filter (containsSame 2)
                |> List.length

        withThree =
            inputList
                |> List.filter (containsSame 3)
                |> List.length
    in
    String.fromInt (withTwo * withThree)


view model =
    div []
        [ div [] [ text "Result:" ]
        , div [] [ text (calculateResult model.input) ]
        ]
