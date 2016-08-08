module Messages exposing (..)

import Models exposing (Operation)


type Msg
    = SetOperation (Maybe Operation)
    | SetOperand (Maybe Float)
    | CalculateResult
