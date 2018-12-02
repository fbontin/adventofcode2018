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


areAlike : String -> String -> Bool
areAlike s1 s2 =
    let
        l1 =
            String.toList s1

        l2 =
            String.toList s2
    in
    List.map2 (\c1 c2 -> c1 /= c2) l1 l2
        |> List.filter (\b -> b)
        |> List.length
        |> (==) 1


calculateResult : String -> String
calculateResult input =
    let
        inputList =
            input
                |> String.lines
                |> List.map String.trim
    in
    inputList
        |> List.filter
            (\s1 ->
                inputList
                    |> List.filter (\s2 -> areAlike s1 s2)
                    |> List.length
                    |> (==) 1
            )
        |> List.intersperse " - "
        |> List.foldl (++) ""


view model =
    div []
        [ div [] [ text "Result:" ]
        , div [] [ text (calculateResult model.input) ]
        ]
