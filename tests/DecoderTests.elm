module DecoderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes exposing (title)
import Json.Decode as D
import Json.Encode as E
import Main exposing (flagsDecoder)
import Random
import Test exposing (..)
import Time
import Todo exposing (Project, Todo, posixDecoder, projectDecoder, projectEncoder, todoDecoder)
import Uuid exposing (Uuid)


makeUuid : Int -> Uuid
makeUuid i =
    Tuple.first <|
        Random.step Uuid.uuidGenerator <|
            Random.initialSeed i


flagsTests : Test
flagsTests =
    test "decodes some flags" <|
        \_ ->
            """{
    "seed": 666,
    "todos": [{"id":"9141f1ca-8740-4b88-a8f4-c138fc19772d","createdAt":1659260320055,"title":"Goodnight, Moon.","description":"The End."},{"id":"1c4e3106-69d7-4020-97d3-9dccd3b8abc3","createdAt":1659260308047,"title":"Hello, World!","description":"He was a dark and stormy knight..."}]
    }"""
                |> D.decodeString flagsDecoder
                |> Expect.ok


todoTests : Test
todoTests =
    describe "Todo"
        [ test "decodes some todos in a list" <|
            \_ ->
                """
                [{"id":"9141f1ca-8740-4b88-a8f4-c138fc19772d","createdAt":1659260320055,"title":"Goodnight, Moon.","description":"The End."},{"id":"1c4e3106-69d7-4020-97d3-9dccd3b8abc3","createdAt":1659260308047,"title":"Hello, World!","description":"He was a dark and stormy knight..."}]
                """
                    |> D.decodeString (D.list projectDecoder)
                    |> Expect.ok
        , fuzz3 int int string "decodes a todo" <|
            \seed time text ->
                Project (makeUuid seed)
                    (Time.millisToPosix time)
                    Nothing
                    text
                    text
                    []
                    |> projectEncoder
                    |> D.decodeValue projectDecoder
                    |> Expect.ok
        , fuzz3 int int string "decodes a todo with task" <|
            \seed time text ->
                Project (makeUuid seed)
                    (Time.millisToPosix time)
                    Nothing
                    text
                    text
                    [ Todo (makeUuid time) text Nothing Nothing ]
                    |> projectEncoder
                    |> D.decodeValue projectDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok text)
        ]


todoTaskTests : Test
todoTaskTests =
    describe "TodoTask Decoders"
        [ test "decodes" <| \_ -> Expect.equal 2 2
        , fuzz2 int string "decodes todotask with no dates" <|
            \seed title ->
                Todo (makeUuid seed) title Nothing Nothing
                    |> Todo.todoEncoder
                    |> D.decodeValue todoDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok title)
        , fuzz3 int int string "decodes todotask wtih start date" <|
            \seed time title ->
                Todo (makeUuid seed)
                    title
                    (time |> Just << Time.millisToPosix)
                    Nothing
                    |> Todo.todoEncoder
                    |> D.decodeValue todoDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok title)
        , fuzz3 int int string "decodes todotask with start & end dates" <|
            \seed time title ->
                Todo (makeUuid seed)
                    title
                    (time |> Just << Time.millisToPosix)
                    (time + seed |> Just << Time.millisToPosix)
                    |> Todo.todoEncoder
                    |> D.decodeValue todoDecoder
                    |> Result.map .title
                    |> Expect.equal (Ok title)
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
