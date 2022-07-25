module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import List
import Random
import Uuid


type alias Todo =
    { id : String, title : String, description : String }


type alias FormData =
    { title : String, description : String }


type ListStyle
    = TodoGrid
    | TodoList


listStyleToClass : ListStyle -> String
listStyleToClass style =
    case style of
        TodoGrid ->
            "grid"

        TodoList ->
            "list"


type alias Model =
    { seed : Random.Seed
    , todos : List Todo
    , displayStyle : ListStyle
    , form : FormData
    }


type Msg
    = GotNewSeed Random.Seed
    | AddTodo
    | NewFormTitle String
    | NewFormDescription String
    | SetDisplayType ListStyle
    | ResetForm
    | DeleteAllTodos


newUuid : Random.Seed -> ( String, Random.Seed )
newUuid seed =
    seed |> Random.step Uuid.uuidGenerator |> Tuple.mapFirst Uuid.toString



-- FORM HELPER FUNCTIONS


emptyForm : FormData
emptyForm =
    { title = ""
    , description = ""
    }


updateFormTitle : String -> FormData -> FormData
updateFormTitle title form =
    { form | title = title }


updateFormDescription : String -> FormData -> FormData
updateFormDescription description form =
    { form | description = description }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        GotNewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        NewFormTitle title ->
            ( { model | form = model.form |> updateFormTitle title }, Cmd.none )

        NewFormDescription description ->
            ( { model | form = model.form |> updateFormDescription description }, Cmd.none )

        SetDisplayType style ->
            case style of
                TodoGrid ->
                    ( { model | displayStyle = TodoGrid }, Cmd.none )

                TodoList ->
                    ( { model | displayStyle = TodoList }, Cmd.none )

        AddTodo ->
            if String.isEmpty model.form.title then
                ( model, Cmd.none )

            else
                let
                    ( id, seed ) =
                        newUuid model.seed

                    { todos } =
                        model

                    { title, description } =
                        model.form
                in
                update ResetForm { model | todos = todos ++ [ { id = id, title = title, description = description } ] }

        ResetForm ->
            ( { model | form = emptyForm }, generateNewSeed )

        DeleteAllTodos ->
            ( { model | todos = [] }, Cmd.none )


generateNewSeed : Cmd Msg
generateNewSeed =
    Random.generate GotNewSeed Random.independentSeed

view : Model -> Html Msg
view model =

    div [ class "container" ]
        [ header []
            [ nav []
                [ button [ onClick (SetDisplayType TodoGrid) ] [ text "Grid" ]
                , button [ onClick (SetDisplayType TodoList) ] [ text "List" ]
                , button [ onClick DeleteAllTodos ] [ text "Delete All" ]
                ]
            ]
        , main_ []
            [ Html.form [ onSubmit AddTodo ]
                [ label []
                    [ span [] [ text "Title" ]
                    , viewInput "text" "Todo title" model.form.title NewFormTitle
                    ]
                , label []
                    [ span [] [ text "Description" ]
                    , viewInput "textarea" "Todo description/notes" model.form.description NewFormDescription
                    ]
                , input [ type_ "submit", value "Add" ] []
                ]
            , div [ class "todo-list" ]
                (if List.isEmpty model.todos then
                    [ text "Add some todos" ]

                 else
                    [ ul [ class ("todo-list " ++ listStyleToClass model.displayStyle) ]
                        (List.map viewTodoItem <| model.todos)
                    ]
                )

viewTodoItem : Todo -> Html msg
viewTodoItem { title, id, description } =
    li [ class "todo-item" ]
        [ h3 [] [ text title ]
        , p [] [ text description ]
        , div [ class "id" ] [ text id ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    case t of
        "textarea" ->
            textarea [ placeholder p, value v, onInput toMsg ] []

        _ ->
            input [ type_ t, placeholder p, value v, onInput toMsg ] []


type alias Flags =
    { seed : Int }


init : Flags -> ( Model, Cmd Msg )
init { seed } =
    ( { seed = Random.initialSeed seed, todos = [], form = { title = "", description = "" }, displayStyle = TodoList }, generateNewSeed )


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
