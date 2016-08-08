module Update exposing (update)

import Models exposing (..)
import Messages exposing (..)


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
