module Models exposing (..)


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
