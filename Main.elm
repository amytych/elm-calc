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
    { operand1 : Maybe Float
    , operand2 : Maybe Float
    , result : Maybe Float
    , operation : Maybe Operation
    , status : Status
    }



-- UPDATE


type Msg
    = SetOperation (Maybe Operation)
    | SetOperand (Maybe Float)
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
                |> resetResult
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


resetResult : Model -> Model
resetResult model =
    { model | result = Nothing }


resetCalc : Model -> Model
resetCalc model =
    { model | operand1 = Nothing, operand2 = Nothing, operation = Nothing }


calculateResult : Model -> Model
calculateResult model =
    case model.operation of
        Nothing ->
            { model | result = Nothing }

        Just operation ->
            performCalculation operation model


performCalculation : Operation -> Model -> Model
performCalculation operation model =
    case ( model.operand1, model.operand2 ) of
        ( Nothing, Nothing ) ->
            model

        ( Nothing, _ ) ->
            model

        ( _, Nothing ) ->
            model

        ( Just operand1, Just operand2 ) ->
            case operation of
                Add ->
                    { model | result = Just ((+) operand1 operand2) }

                Subtract ->
                    { model | result = Just ((-) operand1 operand2) }

                Multiply ->
                    { model | result = Just ((*) operand1 operand2) }

                Divide ->
                    { model | result = Just ((/) operand1 operand2) }


setOperand : Maybe Float -> Model -> Model
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



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { operand1 = Nothing
      , operand2 = Nothing
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
