module Todo exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as E
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
    , todos : Maybe (List Todo)
    }


projectDecoder : D.Decoder Project
projectDecoder =
    D.succeed Project
        |> required "id" Uuid.decoder
        |> required "createdAt" posixDecoder
        |> optional "completedAt" (D.nullable <| posixDecoder) Nothing
        |> required "title" D.string
        |> required "description" D.string
        |> optional "tasks"
            (D.nullable <| D.list todoDecoder)
            Nothing


projectEncoder : Project -> E.Value
projectEncoder =
    \todo ->
        E.object <|
            [ ( "id", Uuid.encode todo.id )
            , ( "createdAt", E.int <| Time.posixToMillis todo.createdAt )
            , ( "title", E.string todo.title )
            , ( "description", E.string todo.description )
            ]


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
