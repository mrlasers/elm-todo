module Todo exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as E
import Time exposing (Posix)
import Uuid exposing (Uuid)


type alias Todo =
    { id : Uuid
    , createdAt : Posix
    , title : String
    , description : String
    , tasks : Maybe (List TodoTask)
    }


posixDecoder : D.Decoder Posix
posixDecoder =
    D.int |> D.andThen (\time -> D.succeed (Time.millisToPosix time))


todosDecoder : D.Decoder (List Todo)
todosDecoder =
    D.list todoDecoder


todoDecoder : D.Decoder Todo
todoDecoder =
    D.succeed Todo
        |> required "id" Uuid.decoder
        |> required "createdAt" posixDecoder
        |> required "title" D.string
        |> required "description" D.string
        |> optional "tasks"
            (D.nullable <| D.list todoTaskDecoder)
            Nothing


todoEncoder : Todo -> E.Value
todoEncoder =
    \todo ->
        E.object <|
            [ ( "id", Uuid.encode todo.id )
            , ( "createdAt", E.int <| Time.posixToMillis todo.createdAt )
            , ( "title", E.string todo.title )
            , ( "description", E.string todo.description )
            ]


type alias TodoTask =
    { id : Uuid
    , title : String
    , start : Maybe Time.Posix
    , end : Maybe Time.Posix
    }


todoTaskDecoder : D.Decoder TodoTask
todoTaskDecoder =
    D.succeed TodoTask
        |> required "id" Uuid.decoder
        |> required "title" D.string
        |> optional "start" (D.nullable posixDecoder) Nothing
        |> optional "end" (D.nullable posixDecoder) Nothing


todoTaskEncoder : TodoTask -> E.Value
todoTaskEncoder task =
    E.object <|
        [ ( "id", Uuid.encode task.id )
        , ( "title", E.string task.title )
        , ( "start"
          , (Maybe.map (Time.posixToMillis >> E.int) <| task.start)
                |> Maybe.withDefault E.null
          )
        , ( "end"
          , (Maybe.map (Time.posixToMillis >> E.int) <| task.end)
                |> Maybe.withDefault E.null
          )
        ]
