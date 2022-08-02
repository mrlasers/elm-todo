module Html.MrLasers exposing (dialog)

import Html exposing (Attribute, Html, button, div, node, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog =
    node "dialog"


configureModal :
    List
        { message : List (Html msg)
        , showWhenMsg : msg
        , onConfirm : msg
        , onCancel : msg
        }
    -> msg
    -> Html msg
configureModal options currentMsg =
    let
        currentDialog =
            options |> List.filter (\option -> option.showWhenMsg == currentMsg) >> List.head
    in
    case currentDialog of
        Just { message, onConfirm, onCancel } ->
            dialog []
                [ div [] message
                , div [ class "dialog-buttons" ]
                    [ button [ onClick onCancel ] []
                    , button [ onClick onConfirm ] [ text "Confirm" ]
                    ]
                ]

        Nothing ->
            Html.text ""
