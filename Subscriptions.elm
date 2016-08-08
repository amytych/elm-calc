module Subscriptions exposing (subscriptions)

import Models exposing (Model)
import Messages exposing (Msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
