port module Main exposing (main)

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


port sendMessage : E.Value -> Cmd msg


sendEncodedMessage : SendMessage -> E.Value
sendEncodedMessage action =
    case action of
        SendText text ->
            E.object [ ( "text", E.string text ) ]

        GetNewId ->
            E.object [ ( "type", E.string "getid" ), ( "payload", E.null ) ]

        SendString type_ text ->
            E.object [ ( "type", E.string type_ ), ( "text", E.string text ) ]


port messageReceiver : (String -> msg) -> Sub msg


type PortMessage
    = FocusInputById String


encodeMessage : PortMessage -> E.Value
encodeMessage msg =
    case msg of
        FocusInputById id ->
            E.object [ ( "type", E.string "focusElement" ), ( "payload", E.string id ) ]


type alias RecvAction =
    { name : String
    , payload : String
    }


recvDecoder : D.Decoder RecvAction
recvDecoder =
    D.map2 RecvAction (D.field "name" D.string) (D.field "payload" D.string)



-- MODEL


type alias Model =
    { seed : Random.Seed
    , todos : List Todo
    , displayStyle : ListStyle
    , form : FormData
    }


type alias Todo =
    { id : String, title : String, description : String }


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
    = GotNewSeed Random.Seed
    | AddTodo
    | NewFormTitle String
    | NewFormDescription String
    | SetDisplayType ListStyle
    | ResetForm
    | DeleteAllTodos
    | Send E.Value
    | Recv String
    | FocusInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    ( id, seed ) =
                        newUuid model.seed

                    { todos } =
                        model

                    { title, description } =
                        model.form
                in
                update ResetForm { model | todos = todos ++ [ { id = id, title = title, description = description } ] }

        ResetForm ->
            ( { model | form = emptyForm }, generateNewSeed )

        DeleteAllTodos ->
            ( { model | todos = [] }, Cmd.none )

        Send encodedMessage ->
            ( model
            , sendMessage encodedMessage
            )

        Recv _ ->
            ( model, Cmd.none )

        FocusInput id ->
            ( model, FocusInputById id |> encodeMessage |> sendMessage )



-- SUBSCRIPTIONS
-- subscriptions : Model -> Sub Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW
-- FORM HELPER FUNCTIONS


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

                -- , button [ onClick (Send (SendText "buttholes")) ] [ text "Send to Port" ]
                , text " | "

                -- , button [ onClick (Send GetNewId) ] [ text "Get new ID" ]
                ]
            ]
        , main_ []
            [ Html.form [ onSubmit AddTodo ]
                [ label []
                    [ span [] [ text "Title" ]
                    , input [ id "task-title", placeholder "Todo title", value model.form.title, onClick (FocusInput "task-title") ] []
                    ]
                , label []
                    [ span [] [ text "Description" ]
                    , textarea [ placeholder "Todo description/notes" ] []

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
    { seed : Int }


init : Flags -> ( Model, Cmd Msg )
init { seed } =
    ( { seed = Random.initialSeed seed, todos = [], form = { title = "", description = "" }, displayStyle = TodoList }, generateNewSeed )


autoSelectInput : List (Attribute msg) -> List () -> Html msg
autoSelectInput attributes _ =
    -- input (attribute "is" "best-text" :: attributes) []
    node "select-input" attributes []
