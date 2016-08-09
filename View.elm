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
            [ button [ onClick (SetOperand "7") ] [ text "7" ]
            , button [ onClick (SetOperand "8") ] [ text "8" ]
            , button [ onClick (SetOperand "9") ] [ text "9" ]
            , button [ onClick (SetOperation (Just Divide)) ] [ text "÷" ]
            ]
        , div []
            [ button [ onClick (SetOperand "4") ] [ text "4" ]
            , button [ onClick (SetOperand "5") ] [ text "5" ]
            , button [ onClick (SetOperand "6") ] [ text "6" ]
            , button [ onClick (SetOperation (Just Multiply)) ] [ text "×" ]
            ]
        , div []
            [ button [ onClick (SetOperand "1") ] [ text "1" ]
            , button [ onClick (SetOperand "2") ] [ text "2" ]
            , button [ onClick (SetOperand "3") ] [ text "3" ]
            , button [ onClick (SetOperation (Just Subtract)) ] [ text "−" ]
            ]
        , div []
            [ button [ onClick (SetOperand "0") ] [ text "0" ]
            , button [ onClick (SetOperand ".") ] [ text "." ]
            , button [ onClick CalculateResult ] [ text "=" ]
            , button [ onClick (SetOperation (Just Add)) ] [ text "+" ]
            ]
        , div [] [ text <| toString model ]
        ]


calcDisplay : Model -> String
calcDisplay model =
    case model.result of
        Nothing ->
            displayedOperand model

        Just result ->
            if isInfinite result then
                "Not a number"
            else
                toString result


displayedOperand : Model -> String
displayedOperand model =
    case ( model.operand1, model.operand2 ) of
        ( Nothing, Nothing ) ->
            "0"

        ( Just operand1, Nothing ) ->
            operand1

        ( _, Just operand2 ) ->
            operand2
