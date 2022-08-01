port module Main exposing (flagsDecoder, main)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as E
import List
import Platform exposing (Task)
import Random
import Task
import Time
import Todo exposing (JobOfWork(..), Project, Todo, projectDecoder, projectEncoder)
import Uuid exposing (Uuid, uuidGenerator)



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
            E.object [ ( "type", E.string "save-todos" ), ( "payload", E.list projectEncoder todos ) ]

        LogToConsole value ->
            case msg of
                _ ->
                    E.object [ ( "type", E.string "console-log" ), ( "payload", value ) ]


type SendPortMessage
    = FocusInputById String
    | SaveTodosList (List Project)
    | LogToConsole E.Value


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


diffTimesInMinutes : Time.Posix -> Time.Posix -> Int
diffTimesInMinutes start end =
    (Time.posixToMillis end - Time.posixToMillis start)
        // 1000



-- // 60
-- 2


type alias Model =
    { seed : Random.Seed
    , jobs : List JobOfWork
    , newJob : String
    , selectedJob : Maybe JobOfWork
    , todos : List Project
    , filteredTodos : List Project
    , todoView : TodoView
    , editing : Maybe Uuid
    , form : FormData
    , jsMessage : String
    , time :
        { now : Time.Posix
        , zone : Time.Zone
        }
    }


type TodoStatus
    = Complete Time.Posix
    | Incomplete



-- should this be Maybe Todo?
-- encodeTodo : Todo -> E.Value
-- encodeTodo todo =
--     E.object
--         [ ( "id", E.string <| Uuid.toString todo.id )
--         , ( "createdAt", E.int (Time.posixToMillis todo.createdAt) )
--         , ( "title", E.string todo.title )
--         , ( "description", E.string todo.description )
--         -- , ( "status"
--         --   , case todo.status of
--         --         Complete date ->
--         --             E.object
--         --                 [ ( "status", E.string "complete" )
--         --                 , ( "date", E.int (Time.posixToMillis date) )
--         --                 ]
--         --         Incomplete ->
--         --             E.object [ ( "status", E.string "incomplete" ) ]
--         --   )
--         , ( "tasks", E.list encodeTodoTask (Maybe.withDefault [] todo.tasks) )
--         ]
-- encodeTodoTask : TodoTask -> E.Value
-- encodeTodoTask task =
--     E.object
--         [ ( "id", Uuid.encode task.id )
--         , ( "start"
--           , case task.start of
--                 Just s ->
--                     E.int (Time.posixToMillis s)
--                 Nothing ->
--                     E.null
--           )
--         , ( "end"
--           , case task.end of
--                 Just s ->
--                     E.int (Time.posixToMillis s)
--                 Nothing ->
--                     E.null
--           )
--         , ( "title", E.string task.title )
--         ]


type alias FormData =
    { id : Maybe Uuid
    , title : String
    , description : String
    , tasks : List Todo
    }


type FormFieldUpdate
    = UpdateTitle String
    | UpdateDescription String
    | AddTodoTask
    | UpdateTaskTitle Int String


type TodoView
    = TodoGrid
    | TodoList


todoViewToString : TodoView -> String
todoViewToString style =
    case style of
        TodoGrid ->
            "grid"

        TodoList ->
            "list"


makeUuid : Random.Seed -> Uuid
makeUuid seed =
    seed |> Random.step uuidGenerator |> Tuple.first


makeFormattedTimeString : { now : Time.Posix, zone : Time.Zone } -> String
makeFormattedTimeString time =
    let
        pad2zero =
            String.padLeft 2 '0'

        hour =
            String.fromInt (Time.toHour time.zone time.now)

        minute =
            pad2zero <| String.fromInt (Time.toMinute time.zone time.now)

        second =
            pad2zero <| String.fromInt (Time.toSecond time.zone time.now)
    in
    hour ++ ":" ++ minute ++ ":" ++ second


makeUTCZuluTimeString : Time.Posix -> String
makeUTCZuluTimeString time =
    Iso8601.fromTime time



-- UPDATE


type Msg
    = Noop
    | GotNewSeed Random.Seed
    | AddTodo
    | UpdateForm FormFieldUpdate
    | SetDisplayType TodoView
    | DeleteTodo Uuid
    | DeleteAllTodos
    | UpdateTodo Uuid TodoUpdate
    | Recv String
    | UpdateJsMessage String
    | Send SendPortMessage
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | AddNewJob
    | UpdateNewJob String
    | SelectJob JobOfWork


type TodoUpdate
    = ToggleComplete
    | AddTask
    | UpdateTask Uuid String
    | DeleteTask Uuid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        AddNewJob ->
            ( { model | newJob = "", jobs = NewJob model.newJob :: model.jobs }, Cmd.none )

        SelectJob job ->
            let
                selectedJob =
                    case job of
                        NewJob str ->
                            StartedJob str model.time.now

                        StartedJob str start ->
                            CompletedJob str start model.time.now (diffTimesInMinutes start model.time.now)

                        CompletedJob _ _ _ _ ->
                            job

                newJobs =
                    model.jobs
                        |> List.map
                            (\j ->
                                if j == job then
                                    selectedJob

                                else
                                    case j of
                                        StartedJob title start ->
                                            CompletedJob title start model.time.now (diffTimesInMinutes start model.time.now)

                                        _ ->
                                            j
                            )
            in
            ( { model
                | selectedJob =
                    if model.selectedJob == Just selectedJob then
                        Nothing

                    else
                        Just job
                , jobs = newJobs
              }
            , Cmd.none
            )

        UpdateNewJob str ->
            ( { model | newJob = str }, Cmd.none )

        Tick newTime ->
            ( model.time
                |> (\time -> { model | time = { time | now = newTime } })
            , Cmd.none
            )

        AdjustTimeZone zone ->
            ( model.time
                |> (\time -> { model | time = { time | zone = zone } })
            , Cmd.none
            )

        GotNewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        UpdateTodo id action ->
            let
                newTodos =
                    case action of
                        AddTask ->
                            model.todos

                        -- |> List.map
                        --     (\todo ->
                        --         if todo.id == id then
                        --             { todo | tasks = todo.tasks ++ [ TodoTask (makeUuid model.seed) "Next Task" Nothing Nothing ] }
                        --         else
                        --             todo
                        --     )
                        DeleteTask taskId ->
                            model.todos

                        -- |> List.map
                        --     (\todo ->
                        --         if todo.id == id then
                        --             { todo | tasks = todo.tasks |> List.filter (\task -> task.id /= taskId) }
                        --         else
                        --             todo
                        --     )
                        UpdateTask taskId value ->
                            model.todos

                        -- |> List.map
                        --     (\todo ->
                        --         if todo.id == id then
                        --             let
                        --                 tasks =
                        --                     todo.tasks
                        --                         |> List.map
                        --                             (\task ->
                        --                                 if task.id == taskId then
                        --                                     { task | title = value }
                        --                                 else
                        --                                     task
                        --                             )
                        --             in
                        --             { todo | tasks = tasks }
                        --         else
                        --             todo
                        --     )
                        _ ->
                            model.todos
            in
            ( { model | todos = newTodos }
            , generateNewSeed
            )

        --         )
        UpdateForm field ->
            let
                { form } =
                    model
            in
            case field of
                UpdateTitle title ->
                    ( { model | form = { form | title = title } }, Cmd.none )

                UpdateDescription desc ->
                    ( { model | form = { form | description = desc } }, Cmd.none )

                UpdateTaskTitle id title ->
                    let
                        x =
                            1
                    in
                    ( model
                    , Cmd.none
                    )

                AddTodoTask ->
                    let
                        id =
                            makeUuid model.seed

                        start =
                            Nothing

                        end =
                            Nothing

                        title =
                            "Untitled Task"
                    in
                    ( { model | form = { form | tasks = [ Todo id title start end ] } }, generateNewSeed )

        SetDisplayType viewType ->
            ( { model | todoView = viewType }, Cmd.none )

        AddTodo ->
            if String.isEmpty model.form.title then
                ( model, Cmd.none )

            else
                let
                    createdAt =
                        model.time.now

                    { title, description } =
                        model.form

                    id =
                        model.form.id
                            |> Maybe.withDefault (makeUuid model.seed)

                    status =
                        Incomplete

                    tasks =
                        model.form.tasks

                    -- if List.isEmpty model.form.tasks then
                    --     Nothing
                    -- else
                    --     Just model.form.tasks
                    todos =
                        Project (Maybe.withDefault (makeUuid model.seed) (Just id)) model.time.now Nothing title description tasks :: model.todos
                in
                ( { model | todos = todos, form = FormData Nothing "" "" [] }
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
            ( { model | todos = [] }
            , messageFromElm (encodeMessage (SaveTodosList []))
            )

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


generateNewSeed : Cmd Msg
generateNewSeed =
    Random.generate GotNewSeed Random.independentSeed


onClickToFocus : String -> Attribute Msg
onClickToFocus =
    onClick << Send << FocusInputById


type SomeShit
    = SomeText String


view : Model -> Document Msg
view model =
    { title = "Tasks.TimothyPew_com"
    , body =
        [ viewHeader model
        , div []
            [ Html.form [ onSubmit AddNewJob ]
                [ input
                    [ onInput UpdateNewJob
                    , value model.newJob
                    ]
                    []
                , button [] [ text "Add shit" ]
                ]
            , ul [ class "jobs" ]
                (model.jobs
                    |> List.map
                        (\job ->
                            let
                                className =
                                    if Just job == model.selectedJob then
                                        "shit"

                                    else
                                        ""
                            in
                            case job of
                                NewJob str ->
                                    li [ class className, onClick <| SelectJob job ] [ text <| "NewJob: " ++ str ]

                                StartedJob str start ->
                                    li [ class className, onClick <| SelectJob job ] [ text <| "Running job: " ++ str ++ " (" ++ (String.fromInt <| diffTimesInMinutes start model.time.now) ++ "s)" ]

                                CompletedJob str _ _ duration ->
                                    li [ class className, onClick <| SelectJob job ] [ text <| "Completed job: " ++ str ++ " (total time: " ++ String.fromInt duration ++ "s)" ]
                        )
                )
            ]
        , main_ []
            [ div [] [ text "put editor here" ]
            , Html.form [ class "new-task", onSubmit AddTodo ]
                [ label []
                    [ span [] [ text "Title" ]
                    , input
                        [ id "task-title"
                        , type_ "text"
                        , autocomplete False
                        , placeholder "Todo0 title"
                        , value model.form.title
                        , onClickToFocus "task-title"
                        , onInput (UpdateForm << UpdateTitle)
                        ]
                        []
                    ]
                , label []
                    [ span [] [ text "Description" ]
                    , textarea
                        [ placeholder "Todo description/notes"
                        , value model.form.description
                        , onInput (UpdateForm << UpdateDescription)
                        ]
                        []
                    ]
                , if List.isEmpty model.form.tasks then
                    text ""

                  else
                    div [] (List.map (\task -> input [ type_ "text", placeholder task.title ] []) model.form.tasks)
                , div [ class "buttons" ]
                    [ input [ type_ "button", value "+", onClick (UpdateForm AddTodoTask) ] []
                    , input [ type_ "submit", value "Add Todo" ] []
                    ]
                ]
            , div [ class "todo-list" ]
                (if List.isEmpty model.todos then
                    [ text "Add some todos, brud!" ]

                 else
                    [ ul [ class ("todo-list " ++ todoViewToString model.todoView) ]
                        (List.map (viewTodoItem model.editing) <| model.todos)
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
            [ case model.todoView of
                TodoList ->
                    button [ onClick (SetDisplayType TodoGrid) ] [ text "View as Grid" ]

                TodoGrid ->
                    button [ onClick (SetDisplayType TodoList) ] [ text "View as List" ]
            , text " | "
            , button [ onClick DeleteAllTodos ] [ text "Delete All" ]
            ]
        , div [] [ text ("Time: " ++ makeFormattedTimeString model.time) ]
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


viewTodoItem : Maybe Uuid -> Project -> Html Msg
viewTodoItem editing { title, id, description, todos } =
    let
        isEditing =
            case editing of
                Just editId ->
                    editId == id

                Nothing ->
                    False
    in
    if isEditing then
        div [] [ text "i'm being edited" ]

    else
        li [ class "todo-item" ]
            [ h3 [] [ text title ]
            , p [] [ text description ]
            , div [ class "id" ] [ text <| Uuid.toString id ]
            , span [ class "delete", onClick (DeleteTodo id) ] [ text "❌" ]
            , div []
                (List.map (\todo -> text todo.title) todos)
            ]


type alias Flags =
    { seed : Int, todos : List Project }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( let
        { seed, todos } =
            case D.decodeValue flagsDecoder flags of
                Ok res ->
                    { seed = Random.initialSeed res.seed
                    , todos = res.todos
                    }

                Err _ ->
                    { seed = Random.initialSeed 666
                    , todos = []
                    }
      in
      { seed = seed
      , newJob = ""
      , selectedJob = Nothing
      , jobs = []
      , todos = todos
      , filteredTodos = todos
      , editing = Nothing
      , form = FormData (Just (makeUuid seed)) "" "" []
      , jsMessage = "ello, world"
      , todoView = TodoList
      , time =
            { now = Time.millisToPosix 0
            , zone = Time.utc
            }
      }
    , Cmd.batch [ generateNewSeed, Task.perform AdjustTimeZone Time.here ]
    )


flagsDecoder : D.Decoder { seed : Int, todos : List Project }
flagsDecoder =
    D.succeed Flags
        |> DP.required "seed" D.int
        |> DP.required "todos" (D.list projectDecoder)



-- D.map2 Flags
--     (D.field "seed" D.int)
--     (D.field "todos" (D.list todoDecoder))
-- intToPosixDecoder : D.Decoder Time.Posix
-- intToPosixDecoder =
--     D.int
--         |> D.andThen
--             (\time -> D.succeed (Time.millisToPosix time))
-- todoDecoder : D.Decoder Todo
-- todoDecoder =
--     D.succeed Todo
--         |> DP.required "id" Uuid.decoder
--         |> DP.required "createdAt" intToPosixDecoder
--         |> DP.required "title" D.string
--         |> DP.required "description" D.string
--         |> DP.optional "tasks" (D.list todoTaskDecoder) []
-- todoTaskDecoder : D.Decoder TodoTask
-- todoTaskDecoder =
--     D.succeed TodoTask
--         |> DP.required "id" Uuid.decoder
--         |> DP.required "title" D.string
--         |> DP.optional "start" (D.nullable intToPosixDecoder) Nothing
--         |> DP.optional "end" (D.nullable intToPosixDecoder) Nothing
