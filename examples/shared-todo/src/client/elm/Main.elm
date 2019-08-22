port module Main exposing (main)

import Browser.Dom as Dom
import CloudModel exposing (localAction, sharedAction, RejectionStrategy(..))
import Html exposing (Html, a, button, div, footer, h1, header, input, label, li, p, section, span, strong, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Json.Encode.Extra
import Task



-- MAIN


main =
    CloudModel.element
        { sharedMsgDecoder = decodeSharedMsg
        , sharedMsgEncoder = encodeSharedMsg
        , displayError = \s -> DisplayError s
        , init = init
        , rejectionStrategy = ReapplyAllPending
        , updateCloud = updateShared
        , updateLocal = updateLocal
        , subscriptions = \_ _ -> Sub.none
        , view = view
        , proposal = proposal
        , proposalResponse = proposalResponse
        , receiveEvents = receiveEvents
        }



-- MODEL


unassignedStringConstant : String
unassignedStringConstant =
    "Unassigned"


{-| The model that is shared across all clients.
-}
type alias SharedModel =
    { todos : List TodoItem
    , uid : Int
    , knownOwners : List Person
    }


type alias TodoItem =
    { description : String
    , completed : Bool
    , id : Int
    , owner : Maybe Person
    }


type alias Person =
    { name : String }


type alias TodoBeingEdited =
    { id : Int
    , newDescription : String
    }


type Visibility
    = VisibilityAll
    | VisibilityActive
    | VisibilityCompleted


visibilityToString : Visibility -> String
visibilityToString visibility =
    case visibility of
        VisibilityAll ->
            "All"

        VisibilityActive ->
            "Active"

        VisibilityCompleted ->
            "Completed"


type OwnerVisibility
    = AssignedToAny
    | Unassigned
    | AssignedTo Person


{-| The model that is local to this individual client.
-}
type alias LocalModel =
    { currentUser : Maybe Person
    , filteringByOwner : OwnerVisibility
    , draft : String
    , todoBeingEdited : Maybe TodoBeingEdited
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
    , knownOwners = []
    }


{-| The initial local model.
-}
initLocalModel : LocalModel
initLocalModel =
    { currentUser = Nothing
    , draft = ""
    , filteringByOwner = AssignedToAny
    , errorMessage = Nothing
    , todoBeingEdited = Nothing
    , visibility = VisibilityAll
    }



-- SHARED UPDATE


{-| Shared messages. These messages update the shared model and we must write
encoders and decoders for them so that they can be communicated over the
network.
-}
type SharedMsg
    = AddTodo TodoItem
    | UpdateTodo Int String
    | DeleteTodo Int
    | AssignTodo Int (Maybe Person)
    | DeleteComplete
    | CheckTodo Int Bool
    | CheckAll Bool
    | AddOwner Person
    | RemoveOwner Person


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
                [ ( "checkTodo"
                  , Json.Encode.object [ ( "id", Json.Encode.int id ), ( "isCompleted", Json.Encode.bool isCompleted ) ]
                  )
                ]

        CheckAll isCompleted ->
            Json.Encode.object
                [ ( "checkAll"
                  , Json.Encode.object [ ( "isCompleted", Json.Encode.bool isCompleted ) ]
                  )
                ]

        AssignTodo id owner ->
            Json.Encode.object
                [ ( "assignTodo"
                  , Json.Encode.object
                        [ ( "id", Json.Encode.int id )
                        , ( "owner", Json.Encode.Extra.maybe encodePerson owner )
                        ]
                  )
                ]

        DeleteTodo id ->
            Json.Encode.object
                [ ( "deleteTodo"
                  , Json.Encode.object [ ( "id", Json.Encode.int id ) ]
                  )
                ]

        DeleteComplete ->
            Json.Encode.object
                [ ( "deleteComplete"
                  , Json.Encode.object []
                  )
                ]

        UpdateTodo id task ->
            Json.Encode.object
                [ ( "updateTodo"
                  , Json.Encode.object [ ( "id", Json.Encode.int id ), ( "task", Json.Encode.string task ) ]
                  )
                ]

        AddOwner owner ->
            Json.Encode.object
                [ ( "addOwner", encodePerson owner )
                ]

        RemoveOwner owner ->
            Json.Encode.object
                [ ( "removeOwner", encodePerson owner )
                ]


encodeTodoItem : TodoItem -> Json.Encode.Value
encodeTodoItem todoItem =
    Json.Encode.object
        [ ( "description", Json.Encode.string todoItem.description )
        , ( "completed", Json.Encode.bool todoItem.completed )
        , ( "id", Json.Encode.int todoItem.id )
        , ( "owner", Json.Encode.Extra.maybe encodePerson todoItem.owner )
        ]


encodePerson : Person -> Json.Encode.Value
encodePerson person =
    Json.Encode.object
        [ ( "name", Json.Encode.string person.name ) ]


{-| Decodes shared messages from the JSON representation written by
`encodeSharedMsg`.
-}
decodeSharedMsg : Decoder SharedMsg
decodeSharedMsg =
    Json.Decode.oneOf
        [ Json.Decode.succeed AddTodo |> required "addTodo" decodeTodoItem
        , Json.Decode.map2
            CheckTodo
            (Json.Decode.at [ "checkTodo", "id" ] Json.Decode.int)
            (Json.Decode.at [ "checkTodo", "isCompleted" ] Json.Decode.bool)
        , Json.Decode.map
            CheckAll
            (Json.Decode.at [ "checkAll", "isCompleted" ] Json.Decode.bool)
        , Json.Decode.map2
            AssignTodo
            (Json.Decode.at [ "assignTodo", "id" ] Json.Decode.int)
            (Json.Decode.at [ "assignTodo", "owner" ] (Json.Decode.maybe decodePerson))
        , Json.Decode.map
            DeleteTodo
            (Json.Decode.at [ "deleteTodo", "id" ] Json.Decode.int)
        , Json.Decode.map
            (always DeleteComplete)
            (Json.Decode.at [ "deleteComplete" ] (Json.Decode.succeed ()))
        , Json.Decode.map2
            UpdateTodo
            (Json.Decode.at [ "updateTodo", "id" ] Json.Decode.int)
            (Json.Decode.at [ "updateTodo", "task" ] Json.Decode.string)
        , Json.Decode.succeed AddOwner |> required "addOwner" decodePerson
        , Json.Decode.succeed RemoveOwner |> required "removeOwner" decodePerson
        ]


decodeTodoItem : Decoder TodoItem
decodeTodoItem =
    Json.Decode.succeed TodoItem
        |> required "description" Json.Decode.string
        |> required "completed" Json.Decode.bool
        |> required "id" Json.Decode.int
        |> required "owner" (Json.Decode.maybe decodePerson)


decodePerson : Decoder Person
decodePerson =
    Json.Decode.succeed Person
        |> required "name" Json.Decode.string


{-| Updates the shared model. Shared model updates are not allowed to issue
commands.
-}
updateShared : SharedMsg -> SharedModel -> SharedModel
updateShared msg model =
    case msg of
        AddTodo newTodo ->
            let newUid = model.uid + 1
                newTodoWithLatestId = { newTodo | id = newUid }
            in
            { model | todos = List.append model.todos [ newTodoWithLatestId ], uid = newUid }

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

        AssignTodo id owner ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | owner = owner }

                    else
                        t
            in
            { model | todos = List.map updateEntry model.todos }

        DeleteTodo id ->
            { model | todos = List.filter (\t -> t.id /= id) model.todos }

        DeleteComplete ->
            { model | todos = List.filter (not << .completed) model.todos }

        UpdateTodo id newTask ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = newTask }

                    else
                        t
            in
            { model | todos = List.map updateEntry model.todos }

        AddOwner newOwner ->
            let
                existingOwner =
                    findOwner newOwner model.knownOwners
            in
            case existingOwner of
                Just owner ->
                    model

                Nothing ->
                    { model | knownOwners = newOwner :: model.knownOwners }

        RemoveOwner existingOwner ->
            let
                filteredOwners =
                    List.filter (\ko -> ko.name /= existingOwner.name) model.knownOwners
            in
            { model | knownOwners = filteredOwners }


findOwner : Person -> List Person -> Maybe Person
findOwner owner knownOwners =
    List.filter (\ko -> ko.name == owner.name) knownOwners
        |> List.head



-- LOCAL UPDATE


{-| Local messages. These messages are applied to the local model only and may
issue commands.
-}
type LocalMsg
    = NoOp
    | DisplayError String
    | UpdateField String
    | StartEditingTodoItem Int String
    | UpdateEditingTodoItem String
    | StopEditingTodoItem
    | ChangeVisibility Visibility
    | ChangeFilteredOwner OwnerVisibility
    | SetCurrentOwner Person


updateLocal : LocalMsg -> LocalModel -> ( LocalModel, Cmd LocalMsg )
updateLocal msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        ChangeFilteredOwner filteringByOwner ->
            ( { model | filteringByOwner = filteringByOwner }, Cmd.none )

        DisplayError error ->
            ( { model | errorMessage = Just error }, Cmd.none )

        StartEditingTodoItem id description ->
            let
                todoBeingEdited =
                    Just { id = id, newDescription = description }

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | todoBeingEdited = todoBeingEdited }, Task.attempt (\_ -> NoOp) focus )

        StopEditingTodoItem ->
            ( { model | todoBeingEdited = Nothing }, Cmd.none )

        UpdateEditingTodoItem newDescription ->
            let
                updatedTodo =
                    case model.todoBeingEdited of
                        Just todoBeingEdited ->
                            Just { id = todoBeingEdited.id, newDescription = newDescription }

                        Nothing ->
                            Nothing
            in
            ( { model | todoBeingEdited = updatedTodo }, Cmd.none )

        UpdateField draft ->
            ( { model | draft = draft }, Cmd.none )

        SetCurrentOwner owner ->
            ( { model | currentUser = Just owner }, Cmd.none )


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
            [ viewInput localModel.draft
            , viewEntries localModel.currentUser localModel.todoBeingEdited localModel.filteringByOwner localModel.visibility sharedModel.todos
            , viewOwners localModel.currentUser localModel.filteringByOwner sharedModel.knownOwners sharedModel.todos
            , lazy2 viewControls localModel.visibility sharedModel.todos
            ]
        , infoFooter
        ]


maybeOwnerToString : Maybe Person -> String
maybeOwnerToString =
    Maybe.map (\owner -> owner.name)
        >> Maybe.withDefault unassignedStringConstant


ownerVisibilityToString : OwnerVisibility -> String
ownerVisibilityToString ownerVisibility =
    case ownerVisibility of
        AssignedToAny ->
            "Any"

        Unassigned ->
            maybeOwnerToString Nothing

        AssignedTo person ->
            maybeOwnerToString (Just person)


errorMessage : Maybe String -> Html msg
errorMessage maybeError =
    case maybeError of
        Just error ->
            Html.text error

        Nothing ->
            Html.text ""


viewInput : String -> Html TodoAction
viewInput task =
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
            , onEnter { localMsg = Just (UpdateField ""), proposedEvent = Just (AddTodo (createNewTodo task)) }
            ]
            []
        ]


createNewTodo : String -> TodoItem
createNewTodo description =
    { description = description, completed = False, id = -1, owner = Nothing }


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


viewEntries : Maybe Person -> Maybe TodoBeingEdited -> OwnerVisibility -> Visibility -> List TodoItem -> Html TodoAction
viewEntries currentUser maybeTodoBeingEdited filteringByOwner visibility entries =
    let
        isVisible todo =
            case visibility of
                VisibilityCompleted ->
                    todo.completed

                VisibilityActive ->
                    not todo.completed

                VisibilityAll ->
                    True

        isVisibleForOwner todo =
            case filteringByOwner of
                AssignedToAny ->
                    True

                Unassigned ->
                    todo.owner == Nothing

                AssignedTo person ->
                    todo.owner == Just person

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
            List.map (viewKeyedTodoItem currentUser maybeTodoBeingEdited) (List.filter (\todo -> isVisible todo && isVisibleForOwner todo) entries)
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedTodoItem : Maybe Person -> Maybe TodoBeingEdited -> TodoItem -> ( String, Html TodoAction )
viewKeyedTodoItem currentUser maybeTodoBeingEdited todo =
    ( String.fromInt todo.id, lazy3 viewTodoItem currentUser maybeTodoBeingEdited todo )


viewTodoItem : Maybe Person -> Maybe TodoBeingEdited -> TodoItem -> Html TodoAction
viewTodoItem currentUser maybeTodoBeingEdited todo =
    let
        ( isEditing, descriptionToShow ) =
            case maybeTodoBeingEdited of
                Nothing ->
                    ( False, todo.description )

                Just todoBeingEdited ->
                    ( todoBeingEdited.id == todo.id, todoBeingEdited.newDescription )

        saveEditTodoAction =
            { localMsg = Just StopEditingTodoItem, proposedEvent = Just (UpdateTodo todo.id descriptionToShow) }
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
            , div
                [ class "todo-details" ]
                [ label
                    [ onDoubleClick (StartEditingTodoItem todo.id todo.description |> localAction) ]
                    [ text todo.description ]
                , div
                    [ classList [ ( "todo-owner", True ), ( "unassigned", todo.owner == Nothing ) ]
                    , onDoubleClick (assignTodo currentUser todo)
                    ]
                    [ text (maybeOwnerToString todo.owner) ]
                ]
            , button
                [ class "destroy"
                , onClick (DeleteTodo todo.id |> sharedAction)
                ]
                []
            ]
        , input
            [ class "edit"
            , value descriptionToShow
            , name "title"
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateEditingTodoItem >> localAction)
            , onBlur saveEditTodoAction
            , onEnter saveEditTodoAction
            ]
            []
        ]


assignTodo : Maybe Person -> TodoItem -> TodoAction
assignTodo currentUser todo =
    let
        assignee =
            if currentUser == todo.owner then
                Nothing

            else
                currentUser
    in
    AssignTodo todo.id assignee |> sharedAction



-- VIEW OWNERS


viewOwners : Maybe Person -> OwnerVisibility -> List Person -> List TodoItem -> Html TodoAction
viewOwners currentUser filteringByOwner knownOwners entries =
    footer
        [ class "owners-footer"
        , hidden (List.isEmpty entries)
        ]
        [ viewCurrentOwner currentUser knownOwners
        , lazy2 viewControlsOwnerFilters filteringByOwner knownOwners
        ]


viewCurrentOwner : Maybe Person -> List Person -> Html TodoAction
viewCurrentOwner currentOwner knownOwners =
    let
        ( ownerNameValue, existingOwner ) =
            case currentOwner of
                Just owner ->
                    ( owner.name, findOwner owner knownOwners )

                Nothing ->
                    ( "", Nothing )
    in
    case existingOwner of
        Nothing ->
            input
                [ class "owner-name"
                , placeholder "Who are you?"
                , autofocus True
                , value ownerNameValue
                , name "currentOwner"
                , onInput (\v -> SetCurrentOwner { name = v } |> localAction)
                , onBlur (createOwner ownerNameValue)
                , onEnter (createOwner ownerNameValue)
                ]
                []

        Just owner ->
            text owner.name


createOwner : String -> TodoAction
createOwner name =
    if String.isEmpty name then
        NoOp |> localAction

    else
        AddOwner { name = name } |> sharedAction


viewControlsOwnerFilters : OwnerVisibility -> List Person -> Html TodoAction
viewControlsOwnerFilters filteringByOwner knownOwners =
    let
        ownerNames =
            List.sortBy .name knownOwners
                |> List.map AssignedTo

        unassigendAndOwners =
            [ AssignedToAny, Unassigned ] ++ ownerNames

        ownerSwaps =
            List.map (\ownerName -> ownerSwap ownerName filteringByOwner) unassigendAndOwners
    in
    ul
        [ class "filters owners-filters" ]
        (List.intersperse (text " ") ownerSwaps)


ownerSwap : OwnerVisibility -> OwnerVisibility -> Html TodoAction
ownerSwap owner filteringByOwner =
    li
        [ onClick (ChangeFilteredOwner owner |> localAction)
        ]
        [ a [ href "#/", classList [ ( "selected", owner == filteringByOwner ) ] ]
            [ text (ownerVisibilityToString owner) ]
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
        , onClick (DeleteComplete |> sharedAction)
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
        ]


port proposal : Json.Encode.Value -> Cmd msg


port proposalResponse : (Json.Decode.Value -> msg) -> Sub msg


port receiveEvents : (Json.Decode.Value -> msg) -> Sub msg
