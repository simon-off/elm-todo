module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, form, h1, input, label, li, main_, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, preventDefaultOn)
import Json.Decode as Json



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { id : Int
    , task : String
    , completed : Bool
    }


type alias Model =
    { todoList : List Todo
    , counter : Int
    , taskInput : String
    }


init : Model
init =
    { todoList =
        [ { id = 1, task = "Learn Elm", completed = False }
        , { id = 2, task = "Build something", completed = False }
        , { id = 3, task = "Profit", completed = False }
        ]
    , counter = 4
    , taskInput = ""
    }



-- UPDATE


type Msg
    = Add String
    | Remove Int
    | Toggle Bool Int
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add taskInput ->
            addTodo taskInput model

        Remove id ->
            removeTodo id model

        Toggle checked id ->
            toggleTodo checked id model

        Input value ->
            { model | taskInput = value }


addTodo : String -> Model -> Model
addTodo taskInput model =
    { model
        | todoList =
            if taskInput /= "" then
                model.todoList
                    ++ [ { id = model.counter, task = taskInput, completed = False } ]

            else
                model.todoList
        , counter = model.counter + 1
        , taskInput = ""
    }


removeTodo : Int -> Model -> Model
removeTodo id model =
    { model
        | todoList =
            List.filter (\todo -> todo.id /= id) model.todoList
    }


toggleTodo : Bool -> Int -> Model -> Model
toggleTodo checked id model =
    { model
        | todoList =
            List.map
                (\todo ->
                    if todo.id == id then
                        { todo | completed = checked }

                    else
                        todo
                )
                model.todoList
    }


onSubmit : msg -> Attribute msg
onSubmit msg =
    preventDefaultOn "submit" (Json.map (\m -> ( m, True )) (Json.succeed msg))



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Elm Todo" ]
        , div []
            [ form [ onSubmit (Add model.taskInput) ]
                [ input [ type_ "text", value model.taskInput, onInput Input ] []
                , button [ disabled (model.taskInput == ""), title "Add todo" ] [ text "➕" ]
                ]
            , ul [] (List.map viewTodo model.todoList)
            ]
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li
        [ class "todo"
        , class
            (if todo.completed then
                "completed"

             else
                ""
            )
        ]
        [ label []
            [ input [ type_ "checkbox", checked todo.completed, onCheck (\checked -> Toggle checked todo.id) ] []
            , span [] [ text todo.task ]
            ]
        , button [ onClick (Remove todo.id), title "Remove todo" ] [ text "🗑️" ]
        ]
