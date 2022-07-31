module TodoDecoder exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as D
import Json.Encode as E
import List
import Task exposing (Task)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- PORTS
-- MODEL


type Status
    = Yatta Todo
    | Boohoo
    | HaventDoneAnythingYet


type alias Model =
    { count : Int
    , json : String
    , todo : Todo
    , status : Status
    }



-- UPDATE


type Msg
    = Noop
    | DecodeJson


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        DecodeJson ->
            let
                decoded =
                    D.decodeString todoDecoder model.json
            in
            ( case decoded of
                Ok todo ->
                    { model | status = Yatta todo }

                Err _ ->
                    { model | status = Boohoo }
            , Cmd.none
            )



-- SUBSCRIPTIONS
-- VIEWW


view : Model -> Html msg
view model =
    div []
        [ nav [] [ button [] [ text "Decode Json" ] ]
        , div [] [ text "He was a dark and stormy knight..." ]
        , div []
            [ text
                (case model.status of
                    Yatta _ ->
                        "YATTA!"

                    Boohoo ->
                        "Boohoo"

                    HaventDoneAnythingYet ->
                        "Pending..."
                )
            ]
        , pre []
            [ code []
                [ text
                    (case D.decodeString todoDecoder todoJson of
                        Ok todo ->
                            let
                                time =
                                    Time.posixToMillis todo.createdAt
                            in
                            "Yatta! :: " ++ String.fromInt time

                        Err error ->
                            D.errorToString error
                    )
                ]
            ]
        , div [] [ div [] [ text ("id: " ++ model.todo.id) ] ]
        ]



-- INIT


todoJson =
    "{\"id\":\"123456\", \"createdAt\":0, \"title\":\"Hello, World!\",\"description\":\"He was a dark and stormy knight...\"}"


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { count = 0
      , json = "{ \"id\":\"1234567890\" }"
      , todo =
            { id = "==="
            , createdAt = Time.millisToPosix 0
            , title = ""
            , description = ""
            }
      , status = HaventDoneAnythingYet
      }
    , Cmd.none
    )



-- DECODERS


type alias Todo =
    { id : String
    , createdAt : Time.Posix
    , title : String
    , description : String
    }


intToPosixDecoder : D.Decoder Time.Posix
intToPosixDecoder =
    D.int
        |> D.andThen
            (\time -> D.succeed (Time.millisToPosix time))


todosDecoder : D.Decoder (List Todo)
todosDecoder =
    D.list todoDecoder


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map4 Todo
        (D.field "id" D.string)
        (D.field "createdAt" D.int
            |> D.andThen
                (\time -> D.succeed (Time.millisToPosix time))
        )
        (D.field "title" D.string)
        (D.field "description" D.string)


todoEncoder : Todo -> E.Value
todoEncoder todo =
    E.object
        [ ( "id", E.string todo.id )
        , ( "createdAt", E.int (Time.posixToMillis todo.createdAt) )
        , ( "title", E.string todo.title )
        , ( "description", E.string todo.description )
        ]


filterResultList : List (Result D.Error Int) -> List Int
filterResultList =
    List.filterMap Result.toMaybe
