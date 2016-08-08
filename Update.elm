module Update exposing (update)

import Models exposing (..)
import Messages exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetOperation operation ->
            ( { model | operation = operation }
                |> ensureFirstOperand
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


{-| When the user sets operation before setting the first operand
in a typical calc the operand defaults to 0, it's a convinience for the user
-}
ensureFirstOperand : Model -> Model
ensureFirstOperand model =
    case ( model.operation, model.operand1 ) of
        ( Just _, Nothing ) ->
            { model | operand1 = Just 0 }

        ( _, _ ) ->
            model


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
