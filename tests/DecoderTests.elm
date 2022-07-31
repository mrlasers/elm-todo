module DecoderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Random exposing (initialSeed, step)
import Test exposing (..)
import Time
import Todo exposing (Todo, posixDecoder, todoDecoder, todoEncoder, todoTaskDecoder)
import Uuid exposing (Uuid)


makeUuid : Int -> Uuid
makeUuid i =
    Tuple.first <|
        Random.step Uuid.uuidGenerator <|
            Random.initialSeed i


todoTests : Test
todoTests =
    fuzz3 int int string "decodes a todo" <|
        \seed time text ->
            Todo (makeUuid seed)
                (Time.millisToPosix time)
                text
                text
                |> todoEncoder
                |> D.decodeValue todoDecoder
                |> Expect.ok


todoTaskTests : Test
todoTaskTests =
    describe "TodoTask Decoders"
        [ test "decodes" <| \_ -> Expect.equal 2 2
        , fuzz2 int string "decodes todotask with no dates" <|
            \seed title ->
                Todo.TodoTask (makeUuid seed) title Nothing Nothing
                    |> Todo.todoTaskEncoder
                    |> D.decodeValue todoTaskDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok title)
        , fuzz3 int int string "decodes todotask wtih start date" <|
            \seed time title ->
                Todo.TodoTask (makeUuid seed) title (time |> Just << Time.millisToPosix) (time + seed |> Just << Time.millisToPosix)
                    |> Todo.todoTaskEncoder
                    |> D.decodeValue todoTaskDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok title)
        , fuzz3 int int string "decodes todotask wtih start & end dates" <|
            \seed time text ->
                [ ( "id", Uuid.encode <| makeUuid seed )
                , ( "title", E.string text )
                , ( "start", E.int time )
                , ( "end", E.int <| time + seed )
                ]
                    |> E.object
                    |> D.decodeValue todoTaskDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok text)
        ]


posixTests : Test
posixTests =
    fuzz int "decodes posix value from int" <|
        \i ->
            E.int i
                |> D.decodeValue posixDecoder
                |> Expect.equal (Ok <| Time.millisToPosix i)


decoderTests : Test
decoderTests =
    describe
        "Json Decoders"
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
