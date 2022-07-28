port module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Iso8601
import Json.Decode as D
import Json.Encode as E
import List
import Random
import Time
import Uuid



-- MAIN


main : Program Flags Model Msg
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
    | SomethingFun String


type alias RecvMessage =
    { type_ : String
    , payload : String
    }


decoderRecv : D.Decoder RecvMessage
decoderRecv =
    D.map2 RecvMessage (D.field "type" D.string) (D.field "payload" D.string)


decodeReceivedMessage : String -> ReceivePortMessage
decodeReceivedMessage incoming =
    D.decodeString decoderRecv incoming
        |> (\result ->
                case result of
                    Ok message ->
                        decodeReceivedPayload message

                    Err error ->
                        UnhandledPortMessage (D.errorToString error)
           )


decodeReceivedPayload : RecvMessage -> ReceivePortMessage
decodeReceivedPayload message =
    let
        { type_, payload } =
            message
    in
    case type_ of
        "something-fun" ->
            SomethingFun payload

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



-- / json decoders
-- todoDecoder : D.Decoder SerializableTodo
-- todoDecoder =
--     D.map3 Todo (D.field "id" D.string) (D.field "title" D.string) (D.field "description" D.string)
-- timeDecoder time =
--     Time.millisToPosix time
-- MODEL


type alias Model =
    { seed : Random.Seed
    , todos : List Todo
    , displayStyle : ListStyle
    , form : FormData
    , jsMessage : String
    , time :
        { now : Time.Posix
        , zone : Time.Zone
        }
    }


type alias SerializableTodo =
    { id : String
    , createdAt : Maybe Int
    , title : String
    , description : String
    }


type alias Todo =
    { id : String
    , createdAt : Time.Posix
    , title : String
    , description : String
    }



-- should this be Maybe Todo?


decodeTodo : D.Decoder Todo
decodeTodo =
    D.map4 Todo
        (D.field "id" D.string)
        (D.field "createdAt" D.int
            |> D.andThen
                (\time ->
                    case time of
                        _ ->
                            D.succeed (Time.millisToPosix 0)
                )
        )
        (D.field "title" D.string)
        (D.field "description" D.string)



-- decodeTodo value =
--     { id = "123"
--     , createdAt =
--         case value.createdAt of
--             Just time ->
--                 Time.millisToPosix time
--             Nothing ->
--                 Time.millisToPosix 0
--     , title = "abc"
--     , description = "Nothing to see here"
--     }


encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.string todo.id )
        , ( "createdAt", E.int (Time.posixToMillis todo.createdAt) )
        , ( "title", E.string todo.title )
        , ( "description", E.string todo.description )
        ]


type alias FormData =
    { id : String

    -- , createdAt : Time.Posix
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



-- 2022-07-26T18:44:37Z


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

                form =
                    model.form

                nextTime =
                    { time | now = newTime }

                nextForm =
                    model.form

                -- { form | createdAt = newTime }
            in
            ( { model | time = nextTime, form = nextForm }, Cmd.none )

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
                            ++ [ { id = id
                                 , createdAt = model.time.now
                                 , title = title
                                 , description = description
                                 }
                               ]
                in
                ( { model | todos = todos, form = FormData (makeUuid model.seed) "" "" }
                , Cmd.batch [ messageFromElm (encodeMessage (SaveTodosList todos)), generateNewSeed ]
                )

        DeleteTodo id ->
            let
                todos =
                    List.filter (\td -> td.id /= id) model.todos
            in
            ( { model | todos = todos }, messageFromElm (encodeMessage (SaveTodosList todos)) )

        DeleteAllTodos ->
            ( { model | todos = [] }, messageFromElm (encodeMessage (SaveTodosList [])) )

        UpdateJsMessage text ->
            ( { model | jsMessage = text }, messageFromElm (E.string "Updated jsMessage...") )

        Recv message ->
            case decodeReceivedMessage message of
                PortString text ->
                    ( { model | jsMessage = text }, Cmd.none )

                SomethingFun text ->
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
        [ header []
            [ nav []
                [ button [ onClick (SetDisplayType TodoGrid) ] [ text "Grid" ]
                , button [ onClick (SetDisplayType TodoList) ] [ text "List" ]
                , text " | "
                , button [ onClick DeleteAllTodos ] [ text "Delete All" ]
                , text " | "
                , button [ onClick (Recv "{ \"type\": \"something-fun\", \"payload\": \"Hello, World!\"}") ] [ text "Recv Success" ]
                , button [ onClick (Recv "{ \"type\": \"important message\", \"payload\": \"very importnat meassage!\"}") ] [ text "Recv Unhandled" ]
                , button [ onClick (Recv "{ type\":d \"something-fun\", \"payload\": \"Hello, World!\"}") ] [ text "Recv Error" ]

                -- , button [ onClick (Send (SendText "buttholes")) ] [ text "Send to Port" ]
                , text " | "

                -- , button [ onClick (Send GetNewId) ] [ text "Get new ID" ]
                ]
            , div [] [ text ("Time: " ++ makeFormattedTimeString model.time.now) ]
            , div [] [ text ("Zulu: " ++ makeUTCZuluTimeString model.time.now) ]
            ]
        , main_ []
            [ Html.form [ onSubmit AddTodo ]
                [ label []
                    [ span [] [ text "Title" ]
                    , input [ id "task-title", placeholder "Todo0 title", onInput NewFormTitle, value model.form.title, onClickToFocus "task-title" ] []
                    ]
                , label []
                    [ span [] [ text "Description" ]
                    , textarea [ placeholder "Todo description/notes", onInput NewFormDescription, value model.form.description ] []

                    -- , viewInput "task-desc" "textarea" "Todo description/notes" model.form.description NewFormDescription
                    ]
                , input [ type_ "submit", value "Add" ] []
                ]
            , div [ class "todo-list" ]
                (case model.todos of
                    [] ->
                        [ text "Add some todos, brud!" ]

                    todos ->
                        [ ul [ class ("todo-list " ++ listStyleToClass model.displayStyle) ]
                            (List.map viewTodoItem <| todos)
                        ]
                )
            ]
        , footer []
            [ div [ class "debug" ]
                [ div []
                    (model.jsMessage
                        |> String.split "\n"
                        |> List.filter (not << String.isEmpty)
                        |> List.map (\line -> p [] [ text line ])
                    )
                ]
            ]
        ]
    }


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


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map2 Flags (D.field "seed" (D.oneOf [ D.int, D.null 0 ])) (D.field "todos" (D.list decodeTodo))


init : Flags -> ( Model, Cmd Msg )
init { seed, todos } =
    let
        newSeed =
            Random.initialSeed seed
    in
    ( { seed = newSeed
      , todos = D.decodeString (D.list decodeTodo) todos
      , form = FormData (makeUuid newSeed) "" ""

      --   , form = makeNewForm newSeed (Time.millisToPosix 0)
      , jsMessage = "[ hello world ]"
      , displayStyle = TodoList
      , time =
            { now = Time.millisToPosix 0
            , zone = Time.utc
            }
      }
    , generateNewSeed
    )
