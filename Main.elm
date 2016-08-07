module Main exposing (..)

import Html exposing (Html, h1, div, button, text)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class)


-- MODEL


type Status
    = ExpectOperand1
    | ExpectOperand2
    | ExpectOperation
    | ExpectResult


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type alias Model =
    { operand1 : Float
    , operand2 : Float
    , operation : Maybe Operation
    , result : Maybe Float
    , status : Status
    }



-- UPDATE


type Msg
    = SetOperation (Maybe Operation)
    | SetOperand Float
    | CalculateResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetOperation operation ->
            ( model
                |> setOperation operation
                |> setStatus
            , Cmd.none
            )

        SetOperand operand ->
            ( model
                |> setOperand operand
                |> setStatus
            , Cmd.none
            )

        CalculateResult ->
            ( model
                |> calculateResult
                |> setStatus
                |> resetCalc
            , Cmd.none
            )


setStatus : Model -> Model
setStatus model =
    case model.status of
        ExpectOperand1 ->
            { model | status = ExpectOperation }

        ExpectOperation ->
            { model | status = ExpectOperand2 }

        ExpectOperand2 ->
            { model | status = ExpectResult }

        ExpectResult ->
            { model | status = ExpectOperand1 }


resetCalc : Model -> Model
resetCalc model =
    { model | operand1 = 0, operand2 = 0, operation = Nothing }


calculateResult : Model -> Model
calculateResult model =
    case model.operation of
        Nothing ->
            { model | result = Nothing }

        Just Add ->
            { model | result = Just (model.operand1 + model.operand2) }

        Just Subtract ->
            { model | result = Just (model.operand1 - model.operand2) }

        Just Multiply ->
            { model | result = Just (model.operand1 * model.operand2) }

        Just Divide ->
            if model.operand2 == 0 then
                { model | result = Nothing }
            else
                { model | result = Just (model.operand1 / model.operand2) }


setOperand : Float -> Model -> Model
setOperand operand model =
    if model.status == ExpectOperand1 then
        { model | operand1 = operand }
    else
        { model | operand2 = operand }


setOperation : Maybe Operation -> Model -> Model
setOperation operation model =
    case operation of
        Nothing ->
            { model | operation = Nothing }

        Just Add ->
            { model | operation = Just Add }

        Just Subtract ->
            { model | operation = Just Subtract }

        Just Multiply ->
            { model | operation = Just Multiply }

        Just Divide ->
            { model | operation = Just Divide }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text <| calcDisplay model ]
        , div []
            [ makeButton 7 (SetOperand 7)
            , makeButton 8 (SetOperand 8)
            , makeButton 9 (SetOperand 9)
            , makeButton "÷" (SetOperation (Just Divide))
            ]
        , div []
            [ makeButton 4 (SetOperand 4)
            , makeButton 5 (SetOperand 5)
            , makeButton 6 (SetOperand 6)
            , makeButton "×" (SetOperation (Just Multiply))
            ]
        , div []
            [ makeButton 1 (SetOperand 1)
            , makeButton 2 (SetOperand 2)
            , makeButton 3 (SetOperand 3)
            , makeButton "−" (SetOperation (Just Subtract))
            ]
        , div []
            [ makeButton 0 (SetOperand 0)
            , makeButton "=" (CalculateResult)
            , makeButton "+" (SetOperation (Just Add))
            ]
        , div [] [ text <| toString model ]
        ]


calcDisplay : Model -> String
calcDisplay model =
    case model.result of
        Nothing ->
            "0"

        Just result ->
            toString result


makeButton : a -> Msg -> Html Msg
makeButton btnText msg =
    button [ onClick msg ] [ text (toString btnText) ]



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { operand1 = 0
      , operand2 = 0
      , operation = Nothing
      , result = Nothing
      , status = ExpectOperand1
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
