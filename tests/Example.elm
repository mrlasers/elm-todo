module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Random exposing (initialSeed, step)
import Test exposing (..)
import Time
import Uuid


decoderTests : Test
decoderTests =
    describe "Json Decoders"
        [ test "decodes a uuid string" <|
            \_ ->
                "\"827470cd-3236-4d07-ae41-ddcd4010b426\""
                    |> D.decodeString Uuid.decoder
                    |> Expect.ok
        , test "decodes a string" <|
            \_ ->
                "\"Hello, World!\""
                    |> D.decodeString D.string
                    |> Expect.equal (Result.Ok "Hello, World!")
        ]


additionTests : Test
additionTests =
    describe "Addition"
        [ test "two plus two equals four" <|
            \_ -> (2 * 2) |> Expect.equal 4
        , test "three plus four equals seven" <|
            \_ -> (3 + 4) |> Expect.equal 7
        ]


guardianNames : Test
guardianNames =
    test "only 2 guardians have names with less than 6 characters" <|
        \_ ->
            let
                guardians =
                    [ "Star-Lord", "Groot", "Gamora", "Drax", "Rocket" ]
            in
            guardians
                |> List.map String.length
                |> List.filter (\x -> x < 6)
                |> List.length
                |> Expect.equal 2
