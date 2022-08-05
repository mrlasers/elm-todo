port module Main exposing (flagsDecoder, main)

-- import Todo exposing (Job(..), Project, Todo, projectDecoder, projectEncoder)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import FontAwesome as FA exposing (icon, plus, trash)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
    exposing
        ( on
        , onBlur
        , onClick
        , onFocus
        , onInput
        , onSubmit
        , stopPropagationOn
        , targetValue
        )
import Html.MrLasers exposing (..)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as E
import List
import Platform exposing (Task)
import Random
import Task
import Time exposing (Posix)
import Uuid exposing (Uuid, uuidGenerator)


type Crud
    = Add
    | Update
    | Remove


if_ : Bool -> c -> c -> c
if_ pred true false =
    if pred then
        true

    else
        false


crudId : Crud -> { a | id : b } -> List { a | id : b } -> List { a | id : b }
crudId =
    crud (\a b -> a.id == b.id)


crud : (a -> a -> Bool) -> Crud -> a -> List a -> List a
crud compare action item list =
    case action of
        Add ->
            item :: list

        Update ->
            list
                |> List.map
                    (\oldItem -> if_ (compare item oldItem) item oldItem)

        Remove ->
            list |> List.filter (\oldItem -> if_ (compare item oldItem) False True)


logString : String -> D.Decoder Msg
logString str =
    D.succeed <| LogString str


decodeMsgBool : msg -> D.Decoder ( msg, Bool )
decodeMsgBool msg =
    D.succeed ( msg, True )


anyMsg : msg -> D.Decoder msg
anyMsg msg =
    D.succeed msg


onClickStop : msg -> Attribute msg
onClickStop message =
    stopPropagationOn "click" (D.map (\msg -> ( msg, True )) (D.succeed message))



-- stopPropagationOn "click" (D.map (\msg -> ( msg, True )) (logString "goddamnit"))
-- Html.Events.stopPropagationOn "click" <| D.map (\a -> ( a, True )) (D.map tagger D.string)
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


sendFromElm : SendPortMessage -> Cmd msg
sendFromElm =
    encodeMessage >> messageFromElm


port messageReceiver : (String -> msg) -> Sub msg


encodeMessage : SendPortMessage -> E.Value
encodeMessage msg =
    case msg of
        FocusInputById id ->
            E.object [ ( "type", E.string "focus-element" ), ( "payload", E.string id ) ]

        SaveProjects projects ->
            E.object
                [ ( "type", E.string "save-projects" )
                , ( "payload", E.list projectEncoder projects )
                ]

        SendShowModal ->
            E.object [ ( "type", E.string "show-modal" ), ( "payload", E.string "modal-dialog" ) ]

        SendHideModal ->
            E.object [ ( "type", E.string "hide-modal" ), ( "payload", E.string "modal-dialog" ) ]

        -- E.object [ ( "type", E.string "save-todos" ), ( "payload", E.list projectEncoder todos ) ]
        LogToConsole value ->
            case msg of
                _ ->
                    E.object [ ( "type", E.string "console-log" ), ( "payload", value ) ]


projectEncoder : Project -> E.Value
projectEncoder project =
    E.object
        [ ( "id", Uuid.encode project.id )
        , ( "title", E.string project.title )
        , ( "description", E.string project.description )
        , ( "todos", E.list todoEncoder <| project.todos )
        ]


projectDecoder : D.Decoder Project
projectDecoder =
    D.succeed Project
        |> DP.required "id" Uuid.decoder
        |> DP.required "title" D.string
        |> DP.optional "description" D.string ""
        |> DP.optional "todos" (D.list todoDecoder) []


todoEncoder : Todo -> E.Value
todoEncoder todo =
    E.object
        [ ( "id", todo.id |> Uuid.encode )
        , ( "title", todo.title |> E.string )
        , ( "timesheet", todo.timesheet |> E.list timesheetEncoder )
        ]


timesheetEncoder : TimeSheet -> E.Value
timesheetEncoder ts =
    case ts of
        ClockIn start ->
            E.object [ ( "start", start |> E.int << Time.posixToMillis ) ]

        ClockOut start end ->
            E.object
                [ ( "start", start |> E.int << Time.posixToMillis )
                , ( "end", end |> E.int << Time.posixToMillis )
                ]


todoDecoder : D.Decoder Todo
todoDecoder =
    D.succeed Todo
        |> DP.required "id" Uuid.decoder
        |> DP.required "title" D.string
        |> DP.optional "timesheet" (D.list timesheetDecoder) []


type alias TimeSheetJson =
    { start : Int
    , end : Maybe Int
    }


timesheetDecoder : D.Decoder TimeSheet
timesheetDecoder =
    D.succeed TimeSheetJson
        |> DP.required "start" D.int
        |> DP.required "end" (D.nullable D.int)
        |> D.andThen
            (\result ->
                case result.end of
                    Just end ->
                        D.succeed (ClockOut (Time.millisToPosix result.start) (Time.millisToPosix end))

                    Nothing ->
                        D.succeed (ClockIn (Time.millisToPosix result.start))
            )


cmdSaveProjects : List Project -> Cmd msg
cmdSaveProjects =
    SaveProjects >> encodeMessage >> messageFromElm


type SendPortMessage
    = FocusInputById String
    | SaveProjects (List Project)
    | LogToConsole E.Value
    | SendShowModal
    | SendHideModal


type ReceivePortMessage
    = PortString String
    | UnhandledPortMessage String


recvPortDecoders : List (D.Decoder ReceivePortMessage)
recvPortDecoders =
    [ D.succeed RecvMessage
        |> DP.required "type" D.string
        |> DP.required "payload" D.string
        |> D.andThen
            (\recv ->
                if recv.type_ == "buttholes" then
                    D.succeed <| PortString recv.payload

                else
                    D.fail "oops, not a butthole"
            )
    ]


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


makeUuid : Random.Seed -> Uuid
makeUuid =
    Random.step uuidGenerator >> Tuple.first


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


type alias Project =
    { id : Uuid
    , title : String
    , description : String
    , todos : List Todo
    }


type alias Todo =
    { id : Uuid, title : String, timesheet : List TimeSheet }


type TimeSheet
    = ClockIn Posix
    | ClockOut Posix Posix


type alias Model =
    { seed : Random.Seed
    , time :
        { now : Time.Posix
        , zone : Time.Zone
        }
    , projects : List Project
    , selectedProject : Maybe Project
    , showModal : KindOfModal
    }


type ProjectField
    = ProjectTitle String
    | ProjectDescription String


type KindOfModal
    = NoModal
    | DeleteAllProjectsModal


type Msg
    = Noop
    | LogString String
    | Recv String
    | GotNewSeed Random.Seed
    | Send SendPortMessage
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | AddNewProject
    | DeleteAllProjects
    | EditProject Project
    | UpdateSelectedProject ProjectField
    | ShowModal KindOfModal
    | AddNewTodo Project
    | UpdateTodo Project TodoUpdate
    | UpdateProject Project



-- | UpdateProjectTodos Project (Todo TodoUpdate)


updateProjects : List Project -> Project -> List Project
updateProjects list project =
    list
        |> List.map
            (\thisProj ->
                if thisProj.id == project.id then
                    project

                else
                    thisProj
            )


updateTodos : List Todo -> Todo -> List Todo
updateTodos todos todo =
    todos
        |> List.map
            (\thisTodo ->
                if thisTodo.id == todo.id then
                    todo

                else
                    thisTodo
            )


type TodoUpdate
    = NewTodo Todo
    | UpdatedTodo Todo
    | DeletedTodo Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, sendFromElm <| LogToConsole (E.string "BOOP") )

        LogString str ->
            ( model, sendFromElm (LogToConsole (E.string str)) )

        Recv message ->
            case decodeReceivedMessage message of
                PortString _ ->
                    ( model, Cmd.none )

                UnhandledPortMessage _ ->
                    ( model, Cmd.none )

        Send message ->
            ( model, messageFromElm (encodeMessage message) )

        GotNewSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        Tick now ->
            ( { model | time = { now = now, zone = model.time.zone } }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | time = { now = model.time.now, zone = zone } }, Cmd.none )

        AddNewProject ->
            let
                id =
                    makeUuid model.seed

                title =
                    "New Project"

                description =
                    ""

                todos =
                    []

                projects =
                    Project id title description todos :: model.projects
            in
            ( { model | projects = projects }
            , Cmd.batch [ cmdSaveProjects projects, cmdGenerateNewSeed ]
            )

        DeleteAllProjects ->
            ( { model | projects = [], selectedProject = Nothing }, Cmd.batch [ cmdSaveProjects model.projects, sendFromElm SendHideModal ] )

        EditProject project ->
            ( { model | selectedProject = if_ (model.selectedProject /= Just project) (Just project) Nothing }
            , Cmd.none
            )

        UpdateSelectedProject field ->
            let
                selectedProject =
                    model.selectedProject
                        |> Maybe.map
                            (\proj ->
                                case field of
                                    ProjectTitle title ->
                                        { proj | title = title }

                                    ProjectDescription desc ->
                                        { proj | description = desc }
                            )

                projects =
                    model.projects
                        |> List.map
                            (\proj ->
                                if Just proj == model.selectedProject then
                                    selectedProject |> Maybe.withDefault proj

                                else
                                    proj
                            )
            in
            ( { model | projects = projects, selectedProject = selectedProject }
              -- , messageFromElm (encodeMessage (SaveProjects projects))
            , cmdSaveProjects projects
            )

        AddNewTodo project ->
            let
                id =
                    makeUuid model.seed

                newProject =
                    { project | todos = Todo id "New Todo" [] :: project.todos }

                newProjects =
                    List.map
                        (\thisProject ->
                            if thisProject == project then
                                newProject

                            else
                                thisProject
                        )
                    <|
                        model.projects

                newSelectedProject =
                    if model.selectedProject == Just project then
                        Just newProject

                    else
                        model.selectedProject
            in
            ( { model | projects = newProjects, selectedProject = newSelectedProject }
            , Cmd.batch [ cmdSaveProjects newProjects, cmdGenerateNewSeed, Task.attempt (\_ -> Noop) (Dom.focus ("todo-title-" ++ Uuid.toString id)) ]
            )

        ShowModal kind ->
            case kind of
                DeleteAllProjectsModal ->
                    ( { model | showModal = kind }, messageFromElm (encodeMessage SendShowModal) )

                _ ->
                    ( { model | showModal = kind }, Cmd.none )

        UpdateTodo project updatedTodo ->
            let
                newTodos =
                    case updatedTodo of
                        NewTodo todo ->
                            crudId Add todo project.todos

                        UpdatedTodo todo ->
                            crudId Update todo project.todos

                        DeletedTodo todo ->
                            crudId Remove todo project.todos

                newProjects =
                    crudId Update { project | todos = newTodos } model.projects
            in
            ( { model | projects = newProjects }, cmdSaveProjects newProjects )

        UpdateProject project ->
            let
                newProjects =
                    updateProjects model.projects project
            in
            ( { model | projects = newProjects }, cmdSaveProjects newProjects )



-- UpdateProjectTodos project updatedTodo ->
--     let
--         todos =
--             project.todos
--         newTodos =
--             case updatedTodo of
--                 NewTodo todo ->
--                     List.map <|
--                         \t ->
--                             if t.id == todo.id then
--                                 todo
--                             else
--                                 t
--         -- newTodos =
--         --     updateTodos project.todos todo
--         newProjects =
--             updateProjects model.projects { project | todos = newTodos }
--     in
--     ( { model | projects = newProjects }, cmdSaveProjects newProjects )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ messageReceiver Recv, Time.every 500 Tick ]


cmdGenerateNewSeed : Cmd Msg
cmdGenerateNewSeed =
    Random.generate GotNewSeed Random.independentSeed


idToOnFocus : String -> Attribute Msg
idToOnFocus =
    onFocus << Send << FocusInputById


view : Model -> Document Msg
view model =
    { title = "Projects"
    , body =
        [ header []
            [ nav []
                [ div [ class "brand" ] [ text "Project Todoer" ] ]
            ]
        , main_ []
            [ case model.selectedProject of
                Just project ->
                    viewProjectEditor project

                Nothing ->
                    div [] [ text "Maybe select a project, brud" ]
            , div [ class "projects-list-container" ]
                [ div [ class "projects-list-header" ]
                    [ h3 [] [ text "Projects" ]
                    , button [ onClick AddNewProject ] [ text "+" ]
                    ]
                , ul [ class "projects-list" ] <|
                    List.map
                        (\project ->
                            viewProjectCard
                                { elem = li
                                , active =
                                    Just project == model.selectedProject
                                , project = project
                                }
                        )
                        model.projects
                ]
            ]
        ]
    }


viewProjectEditor : Project -> Html msg
viewProjectEditor project =
    div [ class "project-editor" ]
        [ h2 [] [ text project.title ]
        , div [] [ p [] [ text project.description  ] ]
        ]


viewProjectCard :
    { elem : List (Attribute Msg) -> List (Html Msg) -> Html Msg
    , active : Bool
    , project : Project
    }
    -> Html Msg
viewProjectCard { elem, active, project } =
    elem
        [ class
            ("project-list-card "
                ++ if_ active "active" ""
            )
        , onClick <| EditProject project
        ]
        [ h4 [] [ text project.title ]
        , if active then
            button [ class "next-todo", onClickStop (LogString "FUCKING YATTA!!!") ]
                [ div []
                    [ div [ class "up-next" ] [ text "Next up..." ]
                    , div [] [ text "Do something new" ]
                    ]
                , button [ class "fa fa-lg fa-fw fa-arrow-circle-right" ] []
                ]

          else
            text ""
        ]


viewOriginal : Model -> Document Msg
viewOriginal model =
    { title = "Tasks.TimothyPew_com"
    , body =
        [ dialog
            [ id "modal-dialog" ]
            (let
                { display, onCancel, onConfirm } =
                    case model.showModal of
                        DeleteAllProjectsModal ->
                            { display = [ text "Really wanna delete all the projects?" ]
                            , onCancel = Send SendHideModal
                            , onConfirm = DeleteAllProjects
                            }

                        _ ->
                            { display = [ text "Nothing to see here" ]
                            , onCancel = Send SendHideModal
                            , onConfirm = Send SendHideModal
                            }
             in
             [ div [] display
             , button [ onClick onCancel ] [ text "Cancel" ]
             , button [ onClick onConfirm ] [ text "Yes, really!!!" ]
             ]
            )
        , div []
            [ button [ onClick AddNewProject ] [ text "+ New Project" ]
            , button [ onClick (ShowModal DeleteAllProjectsModal) ] [ text "Delete All Projects" ]
            ]
        , ul [ class "projects-list" ]
            (List.map
                (\project ->
                    let
                        isEditing =
                            model.selectedProject == Just project

                        className =
                            if_ (model.selectedProject == Just project)
                                "selected"
                                ""

                        title =
                            case project.title of
                                "" ->
                                    "[Untitled Project - think about adding a title]"

                                _ ->
                                    project.title

                        description =
                            project.description
                                |> String.split "\n"
                                >> List.map (\line -> p [] [ text line ])

                        viewTodo todo =
                            div [ class "todo" ]
                                [ input
                                    [ id <| "todo-title-" ++ Uuid.toString todo.id

                                    -- , autofocus True
                                    , class "todo-title"
                                    , value todo.title
                                    , onFocus (Send <| FocusInputById <| "todo-title-" ++ Uuid.toString todo.id)
                                    , onInput (\newTitle -> UpdateTodo project (UpdatedTodo { todo | title = newTitle }))
                                    ]
                                    []
                                , button [ onClick <| UpdateTodo project <| DeletedTodo todo ] [ text "Delete!!!" ]
                                ]

                        viewTodos =
                            div [] <|
                                button [ onClick (AddNewTodo project) ] [ text "Add todo" ]
                                    :: (project.todos
                                            |> List.map
                                                viewTodo
                                       )

                        cardFooter =
                            div [] [ text <| Uuid.toString project.id ]
                    in
                    li [ class className ]
                        [ nav []
                            [ h3 [] [ text title, FA.icon FA.trash ]
                            , div [ class "controls" ]
                                [ button
                                    [ onClick <| EditProject project
                                    , class <| "fa " ++ if_ isEditing "fa-window-close-o" "fa-edit"
                                    ]
                                    []
                                ]
                            , FA.icon FA.trash
                            , i [ class "fa fa-edit" ] []
                            ]
                        , pre [] description
                        , viewTodos
                        , cardFooter
                        ]
                )
                model.projects
            )
        ]
    }


type alias Flags =
    { seed : Int, projects : List Project }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( let
        { seed, projects } =
            Result.withDefault { seed = 666, projects = [] }
                (D.decodeValue flagsDecoder flags)
      in
      { seed = Random.initialSeed seed
      , time =
            { now = Time.millisToPosix 0
            , zone = Time.utc
            }
      , projects = projects
      , selectedProject = Nothing
      , showModal = NoModal
      }
    , Cmd.batch [ cmdGenerateNewSeed, Task.perform AdjustTimeZone Time.here ]
    )


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.succeed Flags
        |> DP.required "seed" D.int
        |> DP.optional "projects" (D.list projectDecoder) []
