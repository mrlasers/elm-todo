port module Main exposing (flagsDecoder, main)

-- import Todo exposing (Job(..), Project, Todo, projectDecoder, projectEncoder)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
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
        [ ( "id", E.string (Uuid.toString project.id) )
        , ( "title", E.string project.title )
        , ( "description", E.string project.description )
        ]


projectDecoder : D.Decoder Project
projectDecoder =
    D.succeed Project
        |> DP.required "id" Uuid.decoder
        |> DP.required "title" D.string
        |> DP.optional "description" D.string ""


saveProjects : List Project -> Cmd msg
saveProjects =
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
    }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

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
                projects =
                    Project (makeUuid model.seed) "New Project" "" :: model.projects
            in
            ( { model | projects = projects }
            , Cmd.batch [ saveProjects projects, cmdGenerateNewSeed ]
            )

        DeleteAllProjects ->
            ( { model | projects = [], selectedProject = Nothing }, saveProjects model.projects )

        EditProject project ->
            ( { model
                | selectedProject =
                    if model.selectedProject == Just project then
                        Nothing

                    else
                        Just project
              }
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
            , saveProjects projects
            )

        ShowModal kind ->
            case kind of
                DeleteAllProjectsModal ->
                    ( { model | showModal = kind }, messageFromElm (encodeMessage SendShowModal) )

                _ ->
                    ( { model | showModal = kind }, Cmd.none )


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
                            , onConfirm = Send SendHideModal
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
        , case model.selectedProject of
            Just proj ->
                div [ class "project-editor" ]
                    [ input
                        [ value proj.title
                        , onInput <| ProjectTitle >> UpdateSelectedProject
                        , id ("title-" ++ Uuid.toString proj.id)
                        , idToOnFocus ("title-" ++ Uuid.toString proj.id)
                        ]
                        []
                    , textarea
                        [ value proj.description
                        , onInput <| ProjectDescription >> UpdateSelectedProject
                        ]
                        []
                    ]

            Nothing ->
                text ""
        , ul [ class "projects-list" ]
            (List.map
                (\project ->
                    let
                        selectedClass =
                            if model.selectedProject == Just project then
                                "selected"

                            else
                                ""
                    in
                    li [ onClick (EditProject project), class selectedClass ]
                        [ h3 []
                            [ text
                                (if String.isEmpty project.title then
                                    "[Untitled Project; think about adding a title]"

                                 else
                                    project.title
                                )
                            ]
                        , pre []
                            (project.description
                                |> String.split "\n"
                                >> List.map (\line -> p [] [ text line ])
                            )
                        , div [] [ text <| Uuid.toString project.id ]
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

        -- newSeed =
        --     D.decodeValue flagsDecoder flags
        --         |> Result.map (\{ seed } -> seed)
        --         |> Result.withDefault 666
        --         |> Random.initialSeed
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
