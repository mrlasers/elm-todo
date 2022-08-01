module TodoTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D exposing (decodeValue)
import Json.Encode as E
import Random
import Test exposing (..)
import Time
import Todo exposing (projectDecoder)
import Uuid


buildUuid integer =
    let
        initialSeed =
            Random.initialSeed integer

        ( uuid, _ ) =
            Random.step Uuid.uuidGenerator initialSeed
    in
    uuid


initialSeedFuzzer =
    Fuzz.map Random.initialSeed Fuzz.int


uuidFuzzer =
    Fuzz.map buildUuid Fuzz.int



-- good uuid: "1d0fc76f-5350-441b-8bfe-335f11fb8c54"


todoTests : Test
todoTests =
    fuzz3 string string int "Todo with no tasks" <|
        \title description createdAt ->
            [ ( "id", E.string "1d0fc76f-5350-441b-8bfe-335f11fb8c54" )
            , ( "title", E.string title )
            , ( "description", E.string description )
            , ( "createdAt", E.int createdAt )
            , ( "tasks", E.list E.string [] )
            ]
                |> E.object
                |> decodeValue projectDecoder
                |> Result.map .title
                |> Expect.equal (Ok title)
