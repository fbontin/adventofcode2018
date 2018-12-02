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



{--input = """abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz""" --}


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


areNotSameChar : Char -> Char -> Bool
areNotSameChar c1 c2 =
    c1 /= c2


areAlike : String -> String -> Bool
areAlike s1 s2 =
    let
        l1 =
            String.toList s1

        l2 =
            String.toList s2
    in
    List.map2 areNotSameChar l1 l2
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

        result =
            inputList
                |> List.filter
                    (\s1 ->
                        inputList
                            |> List.filter (\s2 -> areAlike s1 s2)
                            |> List.length
                            |> (==) 1
                    )
    in
    result
        |> List.intersperse " - "
        |> List.foldl (++) ""


view model =
    div []
        [ div [] [ text "Result:" ]
        , div [] [ text (calculateResult model.input) ]
        ]
