port module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601
import Json.Decode as D
import Json.Encode as E
import List
import Random
import Time
import Uuid



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port messageFromElm : E.Value -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


encodeMessage : SendPortMessage -> E.Value
encodeMessage msg =
    case msg of
        FocusInputById id ->
            E.object [ ( "type", E.string "focus-element" ), ( "payload", E.string id ) ]

        SaveTodosList todos ->
            E.object [ ( "type", E.string "save-todos" ), ( "payload", E.list encodeTodo todos ) ]


type SendPortMessage
    = FocusInputById String
    | SaveTodosList (List Todo)


type ReceivePortMessage
    = PortString String
    | UnhandledPortMessage String


type alias RecvMessage =
    { type_ : String
    , payload : String
    }


decoderRecv : D.Decoder RecvMessage
decoderRecv =
    D.map2 RecvMessage (D.field "type" D.string) (D.field "payload" D.string)


decodeReceivedMessage : String -> ReceivePortMessage
decodeReceivedMessage incoming =
    case D.decodeString decoderRecv incoming of
        Ok message ->
            decodeReceivedPayload message

        Err error ->
            UnhandledPortMessage (D.errorToString error)


decodeReceivedPayload : RecvMessage -> ReceivePortMessage
decodeReceivedPayload { type_, payload } =
    case type_ of
        _ ->
            UnhandledPortMessage
                ("Unknown message: { type: \""
                    ++ type_
                    ++ "\", "
                    ++ "payload: \""
                    ++ payload
                    ++ "\" }"
                    ++ "\n\n"
                    ++ "We should really handle this type of message at some point..."
                )



-- MODEL


type alias Model =
    { seed : Random.Seed
    , todos : List Todo
    , filteredTodos : List Todo
    , displayStyle : ListStyle
    , form : FormData
    , jsMessage : String
    , time :
        { now : Time.Posix
        , zone : Time.Zone
        }
    }


type alias Todo =
    { id : String
    , createdAt : Time.Posix
    , title : String
    , description : String
    }



-- should this be Maybe Todo?


encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.string todo.id )
        , ( "createdAt", E.int (Time.posixToMillis todo.createdAt) )
        , ( "title", E.string todo.title )
        , ( "description", E.string todo.description )
        ]


type alias FormData =
    { id : Maybe String
    , title : String
    , description : String
    }


type ListStyle
    = TodoGrid
    | TodoList


listStyleToClass : ListStyle -> String
listStyleToClass style =
    case style of
        TodoGrid ->
            "grid"

        TodoList ->
            "list"


makeUuid : Random.Seed -> String
makeUuid seed =
    seed |> Random.step Uuid.uuidGenerator |> Tuple.mapFirst Uuid.toString |> Tuple.first


makeFormattedTimeString : Time.Posix -> String
makeFormattedTimeString time =
    String.fromInt (Time.toHour Time.utc time)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt (Time.toMinute Time.utc time))
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt (Time.toSecond Time.utc time))


makeUTCZuluTimeString : Time.Posix -> String
makeUTCZuluTimeString time =
    Iso8601.fromTime time



-- UPDATE


type Msg
    = Noop
    | GotNewSeed Random.Seed
    | AddTodo
    | NewFormTitle String
    | NewFormDescription String
    | SetDisplayType ListStyle
    | DeleteTodo String
    | DeleteAllTodos
    | Recv String
    | UpdateJsMessage String
    | Send SendPortMessage
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Tick newTime ->
            let
                time =
                    model.time
            in
            ( { model | time = { time | now = newTime }, form = model.form }, Cmd.none )

        GotNewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        NewFormTitle title ->
            ( { model | form = model.form |> updateFormTitle title }, Cmd.none )

        NewFormDescription description ->
            ( { model | form = model.form |> updateFormDescription description }, Cmd.none )

        SetDisplayType style ->
            case style of
                TodoGrid ->
                    ( { model | displayStyle = TodoGrid }, Cmd.none )

                TodoList ->
                    ( { model | displayStyle = TodoList }, Cmd.none )

        AddTodo ->
            if String.isEmpty model.form.title then
                ( model, Cmd.none )

            else
                let
                    { id, title, description } =
                        model.form

                    todos =
                        model.todos
                            ++ [ { id = Maybe.withDefault (makeUuid model.seed) id
                                 , createdAt = model.time.now
                                 , title = title
                                 , description = description
                                 }
                               ]
                in
                ( { model | todos = todos, form = FormData Nothing "" "" }
                , Cmd.batch [ messageFromElm (encodeMessage (SaveTodosList todos)), generateNewSeed ]
                )

        DeleteTodo id ->
            let
                newTodos =
                    List.filter (\td -> td.id /= id) model.todos
            in
            ( { model | todos = newTodos }
            , messageFromElm (encodeMessage (SaveTodosList newTodos))
            )

        DeleteAllTodos ->
            ( { model | todos = [] }, messageFromElm (encodeMessage (SaveTodosList [])) )

        UpdateJsMessage text ->
            ( { model | jsMessage = text }, messageFromElm (E.string "Updated jsMessage...") )

        Recv message ->
            case decodeReceivedMessage message of
                PortString text ->
                    ( { model | jsMessage = text }, Cmd.none )

                UnhandledPortMessage text ->
                    ( { model | jsMessage = text }, Cmd.none )

        Send message ->
            ( model, messageFromElm (encodeMessage message) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ messageReceiver Recv, Time.every 1000 Tick ]


updateFormTitle : String -> FormData -> FormData
updateFormTitle title form =
    { form | title = title }


updateFormDescription : String -> FormData -> FormData
updateFormDescription description form =
    { form | description = description }


generateNewSeed : Cmd Msg
generateNewSeed =
    Random.generate GotNewSeed Random.independentSeed


onClickToFocus : String -> Attribute Msg
onClickToFocus =
    onClick << Send << FocusInputById


view : Model -> Document Msg
view model =
    { title = "Tasks.TimothyPew_com"
    , body =
        [ viewHeader model
        , main_ []
            [ Html.form [ onSubmit AddTodo ]
                [ label []
                    [ span [] [ text "Title" ]
                    , input [ id "task-title", placeholder "Todo0 title", onInput NewFormTitle, value model.form.title, onClickToFocus "task-title" ] []
                    ]
                , label []
                    [ span [] [ text "Description" ]
                    , textarea [ placeholder "Todo description/notes", onInput NewFormDescription, value model.form.description ] []
                    ]
                , input [ type_ "submit", value "Add" ] []
                ]
            , div [ class "todo-list" ]
                (if List.isEmpty model.todos then
                    [ text "Add some todos, brud!" ]

                 else
                    [ ul [ class ("todo-list " ++ listStyleToClass model.displayStyle) ]
                        (List.map viewTodoItem <| model.todos)
                    ]
                )
            ]
        , viewFooter model
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ nav []
            [ case model.displayStyle of
                TodoList ->
                    button [ onClick (SetDisplayType TodoGrid) ] [ text "View as Grid" ]

                TodoGrid ->
                    button [ onClick (SetDisplayType TodoList) ] [ text "View as List" ]
            , text " | "
            , button [ onClick DeleteAllTodos ] [ text "Delete All" ]
            ]
        , div [] [ text ("Time: " ++ makeFormattedTimeString model.time.now) ]
        , div [] [ text ("Zulu: " ++ makeUTCZuluTimeString model.time.now) ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    footer []
        [ div [ class "debug" ]
            [ div []
                (model.jsMessage
                    |> String.split "\n"
                    |> List.filter (not << String.isEmpty)
                    |> List.map (\line -> p [] [ text line ])
                )
            ]
        ]


viewTodoItem : Todo -> Html Msg
viewTodoItem { title, id, description } =
    li [ class "todo-item" ]
        [ h3 [] [ text title ]
        , p [] [ text description ]
        , div [ class "id" ] [ text id ]
        , span [ class "delete", onClick (DeleteTodo id) ] [ text "❌" ]
        ]


type alias Flags =
    { seed : Int, todos : List Todo }



-- init : Flags -> ( Model, Cmd Msg )


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( let
        { seed, todos } =
            case D.decodeValue flagDecoder flags of
                Ok res ->
                    { seed = Random.initialSeed res.seed
                    , todos = res.todos
                    }

                Err _ ->
                    { seed = 435356454
                    , todos = []
                    }
      in
      { seed = seed
      , todos = todos
      , filteredTodos = todos
      , form = FormData Nothing "" ""
      , jsMessage = "ello, world"
      , displayStyle = TodoList
      , time =
            { now = Time.millisToPosix 0
            , zone = Time.utc
            }
      }
    , generateNewSeed
    )


flagDecoder : D.Decoder { seed : Int, todos : List Todo }
flagDecoder =
    D.map2 Flags
        (D.field "seed" D.int)
        (D.field "todos" (D.list todoDecoder))


intToPosixDecoder : D.Decoder Time.Posix
intToPosixDecoder =
    D.int
        |> D.andThen
            (\time -> D.succeed (Time.millisToPosix time))


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map4 Todo
        (D.field "id" D.string)
        (D.field "createdAt" intToPosixDecoder)
        (D.field "title" D.string)
        (D.field "description" D.string)
