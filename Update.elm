module Update exposing (update)

import Models exposing (..)
import Messages exposing (..)
import String


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
in a typical calc the operand defaults to 0 or the current result,
if there is any. It's a convenience for the user
-}
ensureFirstOperand : Model -> Model
ensureFirstOperand model =
    case ( model.operation, model.operand1 ) of
        ( Just _, Nothing ) ->
            let
                operand =
                    if model.result /= Nothing then
                        model.result
                    else
                        Just 0
            in
                { model | operand1 = operand }

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


{-| Mimicking the typical calc behavior we need to keep
concatenating the operand1 with each called value, until the user sets
the operation, and then operand2 until the result is calculated
E.g. Operand goes from Nothing to Just 1, after user clicks 1,
then from Just 1 to Just 12 after user clicks 2, and so on…
-}
setOperand : Maybe Float -> Model -> Model
setOperand operand model =
    -- Keep updating first one until we have the operation
    if model.operation == Nothing then
        { model | operand1 = getOperand model.operand1 operand }
        -- then keep updating the second one
    else
        { model | operand2 = getOperand model.operand2 operand }


{-| If we already have a value for the operand, we need to concatenate it with
the new one, otherwise the new one becomes our operand
-}
getOperand : Maybe Float -> Maybe Float -> Maybe Float
getOperand currentOperand newOperand =
    case currentOperand of
        -- No operand yet, just return the new one
        Nothing ->
            newOperand

        -- In case there is a value for it already, try to concatenate it…
        Just currentOp ->
            case newOperand of
                -- …with the new one, first making sure we have it
                Just newOp ->
                    concatOperand currentOp newOp

                -- return current in case we have nothing to concatenate it with
                Nothing ->
                    currentOperand


{-| We need to concatenate the operand, e.g. user clicks 1 followed by 2 should
give us 12
-}
concatOperand : Float -> Float -> Maybe Float
concatOperand currentOp newOp =
    case String.toFloat ((toString currentOp) ++ (toString newOp)) of
        Err _ ->
            Just currentOp

        Ok concatenated ->
            Just concatenated
