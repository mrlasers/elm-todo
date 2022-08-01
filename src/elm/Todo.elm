module Todo exposing (..)

import Json.Decode as D
import Json.Decode.Field as Field
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as E
import Random
import Time exposing (Posix)
import Uuid exposing (Uuid)


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\time -> D.succeed (Time.millisToPosix time))


type alias Project =
    { id : Uuid
    , createdAt : Posix
    , completedAt : Maybe Posix
    , title : String
    , description : String
    , todos : List Todo
    }


projectDecoder : D.Decoder Project
projectDecoder =
    D.succeed Project
        |> required "id" Uuid.decoder
        |> required "createdAt" posixDecoder
        |> optional "completedAt" (D.nullable <| posixDecoder) Nothing
        |> required "title" D.string
        |> required "description" D.string
        |> optional "todos" (D.list todoDecoder) []


projectEncoder : Project -> E.Value
projectEncoder project =
    E.object <|
        [ ( "id", Uuid.encode project.id )
        , ( "createdAt", E.int <| Time.posixToMillis project.createdAt )
        , ( "title", E.string project.title )
        , ( "description", E.string project.description )
        ]


type Job
    = NewJob Uuid String
    | StartedJob Uuid String Posix
    | CompletedJob Uuid String Posix Posix Int


jobEncoder : Job -> E.Value
jobEncoder job =
    let
        props =
            case job of
                NewJob id title ->
                    { status = "new", id = Uuid.toString id }

                StartedJob id title start ->
                    { status = "running", id = Uuid.toString id }

                _ ->
                    { status = "oops", id = "" }
    in
    E.object <| [ ( "status", E.string props.status ) ]


makeUuid : Int -> Uuid
makeUuid i =
    Tuple.first <|
        Random.step Uuid.uuidGenerator <|
            Random.initialSeed i


type alias JobJson =
    { id : Uuid
    , status : String
    , title : String
    , start : Maybe Int
    , end : Maybe Int
    }


dNullableInt : D.Decoder (Maybe Int)
dNullableInt =
    D.nullable D.int


jobDecoder : D.Decoder Job
jobDecoder =
    D.oneOf
        [ D.succeed JobJson
            |> required "id" Uuid.decoder
            |> required "status" D.string
            |> required "title" D.string
            |> optional "start" dNullableInt Nothing
            |> optional "end" dNullableInt Nothing
            |> D.andThen
                (\{ id, status, title, start, end } ->
                    let
                        maybeStart =
                            Maybe.map Time.millisToPosix start

                        maybeEnd =
                            Maybe.map Time.millisToPosix end

                        maybeDuration =
                            Maybe.map2 (-) end start
                    in
                    case ( maybeStart, maybeEnd, maybeDuration ) of
                        ( Nothing, Nothing, Nothing ) ->
                            if status == "new" then
                                D.succeed <| NewJob id title

                            else
                                D.fail "wasn't a new job"

                        ( Just s, Nothing, Nothing ) ->
                            if status == "started" then
                                D.succeed <| StartedJob id title s

                            else
                                D.fail "wasn't a started job"

                        ( Just s, Just e, Just d ) ->
                            if status == "completed" then
                                D.succeed <| CompletedJob id title s e d

                            else
                                D.fail "wasn't a completed job"

                        _ ->
                            D.fail "Something oopsied with your json, brud"
                )
        ]



-- type Todo
--     = Unstarted Uuid
--     | Started Uuid Posix
--     | Complete Uuid Posix Posix Int


type alias Todo =
    { id : Uuid
    , title : String
    , start : Maybe Time.Posix
    , end : Maybe Time.Posix
    }



-- todoTaskDecoder : D.Decoder TodoTask
-- todoTaskDecoder =
--     D.succeed TodoTask
--         |> required "title" D.string


type alias TodoTask =
    { title : String
    , time :
        { start : Maybe Posix
        , end : Maybe Posix
        , total : Maybe Int
        }
    }


todoDecoder : D.Decoder Todo
todoDecoder =
    D.succeed Todo
        |> required "id" Uuid.decoder
        |> required "title" D.string
        |> optional "start" (D.nullable posixDecoder) Nothing
        |> optional "end" (D.nullable posixDecoder) Nothing


todoEncoder : Todo -> E.Value
todoEncoder todo =
    E.object <|
        [ ( "id", Uuid.encode todo.id )
        , ( "title", E.string todo.title )
        , ( "start"
          , (Maybe.map (Time.posixToMillis >> E.int) <| todo.start)
                |> Maybe.withDefault E.null
          )
        , ( "end"
          , (Maybe.map (Time.posixToMillis >> E.int) <| todo.end)
                |> Maybe.withDefault E.null
          )
        ]
