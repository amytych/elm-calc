module Main exposing (..)

import Html.App as App
import Models exposing (..)
import View exposing (view)
import Messages exposing (Msg)
import Update exposing (update)
import Subscriptions exposing (subscriptions)


-- INIT


init : ( Model, Cmd Msg )
init =
    ( { operand1 = Nothing
      , operand2 = Nothing
      , operation = Nothing
      , result = Nothing
      }
    , Cmd.none
    )



-- MAIN


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
