module Update exposing (update)

import Models exposing (..)
import Messages exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetOperation operation ->
            ( { model | operation = operation }
                |> ensureFirstOperand
            , Cmd.none
            )

        SetOperand operand ->
            ( model
                |> setOperand operand
                |> resetResult
            , Cmd.none
            )

        CalculateResult ->
            ( model
                |> calculateResult
                |> resetCalc
            , Cmd.none
            )


{-| When the user sets operation before setting the first operand
in a typical calc the operand defaults to 0, it's a convenience for the user
-}
ensureFirstOperand : Model -> Model
ensureFirstOperand model =
    case ( model.operation, model.operand1 ) of
        ( Just _, Nothing ) ->
            { model | operand1 = Just 0 }

        ( _, _ ) ->
            model


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
    if model.operation == Nothing then
        { model | operand1 = operand }
    else
        { model | operand2 = operand }
