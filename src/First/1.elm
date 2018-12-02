module Main exposing (Model, init, main, update, view)

import Browser
import First.InputData
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
    { input = First.InputData.data }



-- UPDATE


update msg model =
    model



-- VIEW


type Operator
    = Plus
    | Minus


stringToTuple : String -> ( Operator, Int )
stringToTuple row =
    let
        op =
            if String.contains "+" row then
                Plus

            else
                Minus

        value =
            row
                |> String.filter Char.isDigit
                |> String.toInt
                |> Maybe.withDefault 0
    in
    ( op, value )


reduceToValue : ( Operator, Int ) -> ( Operator, Int ) -> ( Operator, Int )
reduceToValue curr acc =
    case curr of
        ( Plus, _ ) ->
            ( Plus, Tuple.second acc + Tuple.second curr )

        ( Minus, _ ) ->
            ( Minus, Tuple.second acc - Tuple.second curr )


calculateSum : String -> String
calculateSum input =
    input
        |> String.split "\n"
        |> List.map String.trim
        |> List.map stringToTuple
        |> List.foldl reduceToValue ( Plus, 0 )
        |> Tuple.second
        |> String.fromInt


view model =
    div []
        [ div [] [ text "Result:" ]
        , div [] [ text (calculateSum model.input) ]
        ]
