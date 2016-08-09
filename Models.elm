module Models exposing (..)


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


type alias Model =
    { operand1 : Maybe String
    , operand2 : Maybe String
    , result : Maybe Float
    , operation : Maybe Operation
    }
