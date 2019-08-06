port module Main exposing (main)

import CloudModel exposing (localAction, sharedAction)
import Html exposing (Html, button, div, input, li, text, ul, footer, p, a, section, header, h1, label, span, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Html.Lazy exposing (lazy, lazy2)
import Html.Keyed as Keyed



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

type Visibility
    = VisibilityAll
    | VisibilityActive
    | VisibilityCompleted

visibilityToString : Visibility -> String
visibilityToString visibility =
    case visibility of
        VisibilityAll -> "All"
        VisibilityActive -> "Active"
        VisibilityCompleted -> "Completed"

{-| The model that is local to this individual client.
-}
type alias LocalModel =
    { draft : String
    , errorMessage : Maybe String
    , visibility : Visibility
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
    , visibility = VisibilityAll
    }



-- SHARED UPDATE


{-| Shared messages. These messages update the shared model and we must write
encoders and decoders for them so that they can be communicated over the
network.
-}
type SharedMsg
    = AddTodo TodoItem
    -- | UpdateTodoItem Int String
    -- | Delete Int
    -- | DeleteComplete
    | CheckTodo Int Bool
    | CheckAll Bool


{-| Encodes shared messages to JSON.
-}
encodeSharedMsg : SharedMsg -> Json.Encode.Value
encodeSharedMsg sharedModelMsg =
    case sharedModelMsg of
        AddTodo todoItem ->
            Json.Encode.object
                [ ( "addTodo", encodeTodoItem todoItem )
                ]
        CheckTodo id isCompleted ->
            Json.Encode.object
                [ ( "checkTodo",
                    Json.Encode.object [ ("id", Json.Encode.int id), ("isCompleted", Json.Encode.bool isCompleted) ] )
                ]
        CheckAll isCompleted ->
            Json.Encode.object
                [ ( "checkAll",
                    Json.Encode.object [ ("isCompleted", Json.Encode.bool isCompleted) ] )
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
        , Json.Decode.map2
            CheckTodo
            (Json.Decode.at ["checkTodo", "id"] Json.Decode.int)
            (Json.Decode.at ["checkTodo", "isCompleted"] Json.Decode.bool)
        , Json.Decode.map
            CheckAll
            (Json.Decode.at ["checkTodo", "isCompleted"] Json.Decode.bool)
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
            { model | todos = List.append model.todos [ newTodo ], uid = newTodo.id + 1 }
        CheckTodo id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }
                    else
                        t
            in
                { model | todos = List.map updateEntry model.todos }
        CheckAll isCompleted -> 
            let
                checkEntry t =
                    { t | completed = isCompleted }
            in
                { model | todos = List.map checkEntry model.todos }





-- LOCAL UPDATE


{-| Local messages. These messages are applied to the local model only and may
issue commands.
-}
type LocalMsg
    = DisplayError String
    | UpdateField String
    -- | EditingTodoItem Int Bool
    | ChangeVisibility Visibility


updateLocal : LocalMsg -> LocalModel -> ( LocalModel, Cmd msg )
updateLocal msg model =
    case msg of
        ChangeVisibility visibility -> ( { model | visibility = visibility}, Cmd.none )
        DisplayError error -> ( { model | errorMessage = Just error }, Cmd.none )
        UpdateField draft ->
            ( { model | draft = draft }, Cmd.none )


type alias TodoAction =
    CloudModel.LocalOriginAction SharedMsg LocalMsg



-- VIEW


view : SharedModel -> LocalModel -> Html TodoAction
view sharedModel localModel =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ errorMessage localModel.errorMessage
        , section
            [ class "todoapp" ]
            [ lazy2 viewInput localModel.draft sharedModel.uid
            , lazy2 viewEntries localModel.visibility sharedModel.todos
            , lazy2 viewControls localModel.visibility sharedModel.todos
            ]
        , infoFooter
        ]

errorMessage : Maybe String -> Html msg
errorMessage maybeError =
    case maybeError of
        Just error -> Html.text error
        Nothing -> Html.text ""

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


viewEntries : Visibility -> List TodoItem -> Html TodoAction
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                VisibilityCompleted ->
                    todo.completed

                VisibilityActive ->
                    not todo.completed

                VisibilityAll ->
                    True

        allCompleted =
            List.all .completed entries

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
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted) |> sharedAction)
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class "todo-list" ] <|
                List.map viewKeyedTodoItem (List.filter isVisible entries)
            ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedTodoItem : TodoItem -> ( String, Html TodoAction )
viewKeyedTodoItem todo =
    ( String.fromInt todo.id, lazy viewTodoItem todo )



viewTodoItem : TodoItem -> Html TodoAction
viewTodoItem todo =
    let isEditing = False
    in
        li
            [ classList [ ( "completed", todo.completed ), ( "editing", isEditing ) ] ]
            [ div
                [ class "view" ]
                [ input
                    [ class "toggle"
                    , type_ "checkbox"
                    , checked todo.completed
                    , onClick (CheckTodo todo.id (not todo.completed) |> sharedAction)
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


viewControls : Visibility -> List TodoItem -> Html TodoAction
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

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


viewControlsFilters : Visibility -> Html TodoAction
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" VisibilityAll visibility
        , text " "
        , visibilitySwap "#/active" VisibilityActive visibility
        , text " "
        , visibilitySwap "#/completed" VisibilityCompleted visibility
        ]


visibilitySwap : String -> Visibility -> Visibility -> Html TodoAction
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility |> localAction) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text (visibilityToString visibility) ]
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
