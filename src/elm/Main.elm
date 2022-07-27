port module Main exposing (main)

import Array as A
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import List
import Random
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


type SendMessage
    = SendText String
    | GetNewId
    | SendString String String


port messageFromElm : E.Value -> Cmd msg


sendEncodedMessage : SendMessage -> E.Value
sendEncodedMessage message =
    case message of
        SendText text ->
            E.object [ ( "text", E.string text ) ]

        GetNewId ->
            E.object [ ( "type", E.string "getid" ), ( "payload", E.null ) ]

        SendString type_ text ->
            E.object [ ( "type", E.string type_ ), ( "text", E.string text ) ]


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



-- type alias MessagePayloadDecoder =
--     { type_ : String
--     , payload : String
--     }
-- handleReceivedMessage : String -> Msg
-- handleReceivedMessage message =
--     recvDecoder message
--         |> (\received ->
--                 case received.type_ of
--                     "jsmessage" ->
--                         UpdateJsMessage "got a jsmessage"
--                     _ ->
--                         Noop
--            )
-- D.map2 RecvAction (D.field "type_" D.string) (D.field "payload" D.string)
--     |> (\action ->
--             case action.type_ of
--                 _ ->
--                     Noop
--        )
-- MODEL


type alias Model =
    { seed : Random.Seed
    , todos : List Todo
    , displayStyle : ListStyle
    , form : FormData
    , jsMessage : String
    }


type alias Todo =
    { id : String, title : String, description : String }


encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.string todo.id )
        , ( "title", E.string todo.title )
        , ( "description", E.string todo.description )
        ]


type alias FormData =
    { title : String, description : String }


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


newUuid : Random.Seed -> ( String, Random.Seed )
newUuid seed =
    seed |> Random.step Uuid.uuidGenerator |> Tuple.mapFirst Uuid.toString



-- UPDATE


type Msg
    = Noop
    | GotNewSeed Random.Seed
    | AddTodo
    | NewFormTitle String
    | NewFormDescription String
    | SetDisplayType ListStyle
    | DeleteAllTodos
    | Recv String
    | UpdateJsMessage String
    | Send SendPortMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

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
                    ( id, _ ) =
                        newUuid model.seed

                    { title, description } =
                        model.form

                    todos =
                        model.todos ++ [ { id = id, title = title, description = description } ]
                in
                ( { model | todos = todos, form = emptyForm }, messageFromElm (encodeMessage (SaveTodosList todos)) )

        DeleteAllTodos ->
            ( { model | todos = [] }, Cmd.none )

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
    messageReceiver Recv


emptyForm : FormData
emptyForm =
    { title = ""
    , description = ""
    }


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
                (if List.isEmpty model.todos then
                    [ text "Add some todos" ]

                 else
                    [ ul [ class ("todo-list " ++ listStyleToClass model.displayStyle) ]
                        (List.map viewTodoItem <| model.todos)
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


viewTodoItem : Todo -> Html msg
viewTodoItem { title, id, description } =
    li [ class "todo-item" ]
        [ h3 [] [ text title ]
        , p [] [ text description ]
        , div [ class "id" ] [ text id ]
        ]


type alias Flags =
    { seed : Int, todos : List Todo }


init : Flags -> ( Model, Cmd Msg )
init { seed, todos } =
    ( { seed = Random.initialSeed seed
      , todos = todos
      , form = { title = "", description = "" }
      , jsMessage = "[ hello world ]"
      , displayStyle = TodoList
      }
    , generateNewSeed
    )


autoSelectInput : List (Attribute msg) -> List () -> Html msg
autoSelectInput attributes _ =
    -- input (attribute "is" "best-text" :: attributes) []
    node "select-input" attributes []
