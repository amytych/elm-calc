module View exposing (view)

import Html exposing (Html, h1, div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Models exposing (..)
import Messages exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text <| calcDisplay model ]
        , div []
            [ makeButton 7 (SetOperand (Just 7))
            , makeButton 8 (SetOperand (Just 8))
            , makeButton 9 (SetOperand (Just 9))
            , makeButton "÷" (SetOperation (Just Divide))
            ]
        , div []
            [ makeButton 4 (SetOperand (Just 4))
            , makeButton 5 (SetOperand (Just 5))
            , makeButton 6 (SetOperand (Just 6))
            , makeButton "×" (SetOperation (Just Multiply))
            ]
        , div []
            [ makeButton 1 (SetOperand (Just 1))
            , makeButton 2 (SetOperand (Just 2))
            , makeButton 3 (SetOperand (Just 3))
            , makeButton "−" (SetOperation (Just Subtract))
            ]
        , div []
            [ makeButton 0 (SetOperand (Just 0))
            , makeButton "=" (CalculateResult)
            , makeButton "+" (SetOperation (Just Add))
            ]
        , div [] [ text <| toString model ]
        ]


calcDisplay : Model -> String
calcDisplay model =
    case model.result of
        Nothing ->
            case ( model.operand1, model.operand2 ) of
                ( Nothing, Nothing ) ->
                    "0"

                ( Just operand1, Nothing ) ->
                    toString operand1

                ( _, Just operand2 ) ->
                    toString operand2

        Just result ->
            if isInfinite result then
                "Not a number"
            else
                toString result


makeButton : a -> Msg -> Html Msg
makeButton btnText msg =
    button [ onClick msg ] [ text (toString btnText) ]
