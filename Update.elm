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

        SetOperand partial ->
            ( model
                |> setOperand partial
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
                    case model.result of
                        Nothing ->
                            Just "0"

                        Just result ->
                            Just <| toString result
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
            case ( model.operand1, model.operand2 ) of
                ( Just operand1, Just operand2 ) ->
                    { model | result = performCalculation operation operand1 operand2 }

                ( _, _ ) ->
                    model


performCalculation : Operation -> String -> String -> Maybe Float
performCalculation operation operand1 operand2 =
    case ( String.toFloat operand1, String.toFloat operand2 ) of
        ( Ok op1, Ok op2 ) ->
            case operation of
                Add ->
                    Just ((+) op1 op2)

                Subtract ->
                    Just ((-) op1 op2)

                Multiply ->
                    Just ((*) op1 op2)

                Divide ->
                    Just ((/) op1 op2)

        ( _, _ ) ->
            Nothing


{-| Mimicking the typical calc behavior we need to keep
concatenating the operand1 with each called value, until the user sets
the operation, and then operand2 until the result is calculated
E.g. Operand goes from Nothing to Just "1", after user clicks "1",
then from Just "1" to Just "12" after user clicks "2", and so on…
Also we need to account for decimals after user clicks "."
-}
setOperand : String -> Model -> Model
setOperand partial model =
    if model.operation == Nothing then
        { model | operand1 = getOperand model.operand1 partial }
    else
        { model | operand2 = getOperand model.operand2 partial }


{-| If we already have a value for the operand, we need to concatenate it with
the new one, otherwise the new one becomes our operand, with special handling
for decimal "." and "0" as
-}
getOperand : Maybe String -> String -> Maybe String
getOperand current partial =
    let
        currentString =
            case current of
                Nothing ->
                    ""

                Just operand ->
                    operand
    in
        case partial of
            "." ->
                if String.contains "." currentString then
                    Just currentString
                else
                    Just (currentString ++ partial)

            "0" ->
                if currentString == "0" then
                    Just currentString
                else
                    Just (currentString ++ partial)

            _ ->
                Just (currentString ++ partial)
