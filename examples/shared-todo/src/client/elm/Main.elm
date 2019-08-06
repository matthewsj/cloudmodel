port module Main exposing (main)

import CloudModel exposing (localAction)
import Html exposing (Html, button, div, input, li, text, ul, footer, p, a, section, header, h1, label, span, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Html.Lazy exposing (lazy, lazy2)



-- MAIN


main =
    CloudModel.element
        { sharedMsgDecoder = decodeSharedMsg
        , sharedMsgEncoder = encodeSharedMsg
        , displayError = \s -> DisplayError s
        , init = init
        , updateCloud = updateShared
        , updateLocal = updateLocal
        , subscriptions = \_ _ -> Sub.none
        , view = view
        , proposal = proposal
        , proposalResponse = proposalResponse
        , receiveEvents = receiveEvents
        }



-- MODEL


{-| The model that is shared across all clients.
-}
type alias SharedModel =
    { todos : List TodoItem
    , uid : Int
    }


type alias TodoItem =
    { description : String
    , completed : Bool
    , id : Int
    }

{-| The model that is local to this individual client.
-}
type alias LocalModel =
    { draft : String
    , errorMessage : Maybe String
    , visibility : String -- TODO: Better type
    }



-- INIT


{-| Initial values for shared and local models as well as local commands to
run on start.
-}
init : () -> ( SharedModel, LocalModel, Cmd LocalMsg )
init flags =
    ( initSharedModel
    , initLocalModel
    , Cmd.none
    )


{-| The initial shared model.
-}
initSharedModel : SharedModel
initSharedModel =
    { todos = []
    , uid = 0
    }


{-| The initial local model.
-}
initLocalModel : LocalModel
initLocalModel =
    { draft = ""
    , errorMessage = Nothing
    , visibility = "All"
    }



-- SHARED UPDATE


{-| Shared messages. These messages update the shared model and we must write
encoders and decoders for them so that they can be communicated over the
network.
-}
type SharedMsg
    = AddTodo TodoItem


{-| Encodes shared messages to JSON.
-}
encodeSharedMsg : SharedMsg -> Json.Encode.Value
encodeSharedMsg sharedModelMsg =
    case sharedModelMsg of
        AddTodo todoItem ->
            Json.Encode.object
                [ ( "addTodo", encodeTodoItem todoItem )
                ]

encodeTodoItem : TodoItem -> Json.Encode.Value
encodeTodoItem todoItem =
    Json.Encode.object
    [ ("description", Json.Encode.string todoItem.description )
    , ("completed", Json.Encode.bool todoItem.completed )
    , ("id", Json.Encode.int todoItem.id )]

{-| Decodes shared messages from the JSON representation written by
`encodeSharedMsg`.
-}
decodeSharedMsg : Decoder SharedMsg
decodeSharedMsg =
    Json.Decode.oneOf
        [ Json.Decode.succeed AddTodo |> required "addTodo" decodeTodoItem
        ]


decodeTodoItem : Decoder TodoItem
decodeTodoItem =
    Json.Decode.succeed TodoItem
        |> required "description" Json.Decode.string
        |> required "completed" Json.Decode.bool
        |> required "id" Json.Decode.int


{-| Updates the shared model. Shared model updates are not allowed to issue
commands.
-}
updateShared : SharedMsg -> SharedModel -> SharedModel
updateShared msg model =
    case msg of
        AddTodo newTodo ->
            Debug.log "newModel" { model | todos = List.append model.todos [ newTodo ], uid = newTodo.id + 1 }



-- LOCAL UPDATE


{-| Local messages. These messages are applied to the local model only and may
issue commands.
-}
type LocalMsg
    = DisplayError String
    | UpdateField String
    -- | EditingTodoItem Int Bool
    -- | UpdateTodoItem Int String
    -- | Add
    -- | Delete Int
    -- | DeleteComplete
    -- | Check Int Bool
    -- | CheckAll Bool
    -- | ChangeVisibility String


updateLocal : LocalMsg -> LocalModel -> ( LocalModel, Cmd msg )
updateLocal msg model =
    case msg of
        UpdateField draft ->
            ( { model | draft = draft }, Cmd.none )
        DisplayError error -> ( { model | errorMessage = Just error }, Cmd.none )


type alias TodoAction =
    CloudModel.LocalOriginAction SharedMsg LocalMsg



-- VIEW


view : SharedModel -> LocalModel -> Html TodoAction
view sharedModel localModel =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy2 viewInput localModel.draft sharedModel.uid
            , lazy2 viewEntries localModel.visibility sharedModel.todos
            , lazy2 viewControls localModel.visibility sharedModel.todos
            ]
        , infoFooter
        ]


viewInput : String -> Int -> Html TodoAction
viewInput task uid =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput (UpdateField >> localAction)
            , onEnter { localMsg = Just (UpdateField ""), proposedEvent = Just (AddTodo (createNewTodo task uid)) }
            ]
            []
        ]

createNewTodo : String -> Int -> TodoItem
createNewTodo description id =
    { description = description, completed = False, id = id }


onEnter : TodoAction -> Html.Attribute TodoAction
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        Html.Events.on "keydown" (Json.Decode.andThen isEnter Html.Events.keyCode)



-- VIEW ALL ENTRIES


viewEntries : String -> List TodoItem -> Html TodoAction
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        -- allCompleted =
        --     List.all .completed entries

        allCompleted = entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style "visibility" cssVisibility
            ]
            []
            -- [ input
            --     [ class "toggle-all"
            --     , type_ "checkbox"
            --     , name "toggle"
            --     , checked allCompleted
            --     , onClick (CheckAll (not allCompleted))
            --     ]
            --     []
            -- , label
            --     [ for "toggle-all" ]
            --     [ text "Mark all as complete" ]
            -- , Keyed.ul [ class "todo-list" ] <|
            --     List.map viewKeyedTodoItem (List.filter isVisible entries)
            -- ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedTodoItem : TodoItem -> ( String, Html TodoAction )
viewKeyedTodoItem todo =
    ( String.fromInt todo.id, lazy viewTodoItem todo )



viewTodoItem : TodoItem -> Html TodoAction
viewTodoItem todo =
    li
        -- [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ classList [ ( "completed", False ), ( "editing", False ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked False
                -- , onClick (Check todo.id (not todo.completed))
                ]
                []
            , label
              []
                -- [ onDoubleClick (EditingTodoItem todo.id True) ]
                [ text todo.description ]
            , button
                [ class "destroy"
                -- , onClick (Delete todo.id)
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ String.fromInt todo.id)
            -- , onInput (UpdateTodoItem todo.id)
            -- , onBlur (EditingTodoItem todo.id False)
            -- , onEnter (EditingTodoItem todo.id False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List TodoItem -> Html TodoAction
viewControls visibility entries =
    let
        entriesCompleted =
            List.length entries

        entriesLeft =
            List.length entries - entriesCompleted
    in
        footer
            [ class "footer"
            , hidden (List.isEmpty entries)
            ]
            [ lazy viewControlsCount entriesLeft
            , lazy viewControlsFilters visibility
            , lazy viewControlsClear entriesCompleted
            ]


viewControlsCount : Int -> Html TodoAction
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"
            else
                " items"
    in
        span
            [ class "todo-count" ]
            [ strong [] [ text (String.fromInt entriesLeft) ]
            , text (item_ ++ " left")
            ]


viewControlsFilters : String -> Html TodoAction
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html TodoAction
visibilitySwap uri visibility actualVisibility =
    li
        []
        -- [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Int -> Html TodoAction
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        -- , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Adapted from "
            , a [ href "https://github.com/evancz/elm-todomvc" ] [ text "Evan Czaplicki's Elm TODO MVC" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]


port proposal : Json.Encode.Value -> Cmd msg


port proposalResponse : (Json.Decode.Value -> msg) -> Sub msg


port receiveEvents : (Json.Decode.Value -> msg) -> Sub msg
