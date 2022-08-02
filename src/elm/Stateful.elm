module Stateful exposing (..)

import Browser
import Html exposing (..)


stateful : Model -> Html msg
stateful model =
    div [] [ text <| "Stateful" ++ String.fromInt model ]


type alias Model =
    Int


main : Program () Model msg
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = \model -> div [] [ text "Stateful component" ]
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
