port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ proposalResponse (decodeOrFail (Json.Decode.map ControlMsg (decodeProposalResponse decodeSharedModelMsg)))
        , receiveEvents (decodeOrFail (Json.Decode.map RemoteOrigin (Json.Decode.list (decodeEvent decodeSharedModelMsg))))
        ]


decodeProposalResponse : Decoder sharedModelMsg -> Decoder (ControlMsg sharedModelMsg)
decodeProposalResponse decodeSharedMsg =
    oneOf
        [ field "accept" decodeAccept
        , field "reject" (decodeReject decodeSharedMsg)
        ]


decodeAccept : Decoder (ControlMsg sharedModelMsg)
decodeAccept =
    succeed Accept
        |> required "eventId" Json.Decode.int
        |> required "clientEventId" Json.Decode.int


decodeReject : Decoder sharedModelMsg -> Decoder (ControlMsg sharedModelMsg)
decodeReject decodeSharedMsg =
    succeed Reject
        |> required "clientEventId" Json.Decode.int
        |> required "missingEvents" (Json.Decode.list (decodeEvent decodeSharedMsg))


decodeEvent : Decoder sharedModelMsg -> Decoder (Event sharedModelMsg)
decodeEvent decodeSharedMsg =
    succeed Event
        |> required "msg" decodeSharedMsg
        |> required "id" Json.Decode.int


decodeSharedModelMsg : Decoder SharedChatMsg
decodeSharedModelMsg =
    Json.Decode.oneOf
        [ succeed AddChat |> required "addChat" Json.Decode.string
        ]


decodeOrFail : Decoder Msg -> Json.Decode.Value -> Msg
decodeOrFail decoder json =
    case Json.Decode.decodeValue decoder json of
        Ok msg ->
            msg

        Err error ->
            LocalOrigin (localAction (DisplayError (Json.Decode.errorToString error)))



-- MODEL
-- generic model stuff


type alias EventId =
    Int


type alias ClientEventId =
    Int


type alias CloudModel sharedModel sharedModelMsg localModel =
    { sharedModelInfo : SharedModelInfo sharedModel sharedModelMsg
    , localModel : localModel
    }


type alias SharedModelInfo sharedModel sharedModelMsg =
    { latestKnownEventId : EventId
    , latestKnownSharedModel : sharedModel
    , pendingEvents : List (Event sharedModelMsg)
    }



-- chat-specific model stuff


type alias Model =
    CloudModel SharedChatModel SharedChatMsg LocalChatModel


type alias SharedChatModel =
    { chats : List String
    }


type alias LocalChatModel =
    { draft : String
    , errorMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sharedModelInfo = initSharedModelInfo initSharedModel
      , localModel = initLocalModel
      }
    , Cmd.none
    )


initSharedModelInfo : sharedModel -> SharedModelInfo sharedModel sharedModelMsg
initSharedModelInfo initLatestKnownSharedModel =
    { latestKnownEventId = 0
    , latestKnownSharedModel = initLatestKnownSharedModel
    , pendingEvents = []
    }


initSharedModel : SharedChatModel
initSharedModel =
    { chats = []
    }


initLocalModel : LocalChatModel
initLocalModel =
    { draft = ""
    , errorMessage = Nothing
    }



-- UPDATE
-- generic update stuff


type alias Event sharedModelMsg =
    { msg : sharedModelMsg
    , id : EventId
    }


type alias LocalOriginAction sharedModelMsg localModelMsg =
    { localMsg : Maybe localModelMsg
    , proposedEvent : Maybe sharedModelMsg
    }


type CloudMsg sharedModelMsg localModelMsg
    = LocalOrigin (LocalOriginAction sharedModelMsg localModelMsg)
    | RemoteOrigin (List (Event sharedModelMsg))
    | ControlMsg (ControlMsg sharedModelMsg)


type ControlMsg sharedModelMsg
    = Accept EventId ClientEventId
    | Reject ClientEventId (List (Event sharedModelMsg))


localAction : localModelMsg -> LocalOriginAction sharedModelMsg localModelMsg
localAction localMsg =
    { localMsg = Just localMsg, proposedEvent = Nothing }


sharedAction : sharedModelMsg -> LocalOriginAction sharedModelMsg localModelMsg
sharedAction sharedModelMsg =
    { localMsg = Nothing, proposedEvent = Just sharedModelMsg }


constructCloudUpdate :
    (sharedModelMsg -> Json.Encode.Value)
    -> (sharedModelMsg -> sharedModel -> sharedModel)
    -> (localModelMsg -> localModel -> ( localModel, Cmd localModelMsg ))
    -> CloudMsg sharedModelMsg localModelMsg
    -> CloudModel sharedModel sharedModelMsg localModel
    -> ( CloudModel sharedModel sharedModelMsg localModel, Cmd (CloudMsg sharedModelMsg localModelMsg) )
constructCloudUpdate sharedModelMsgEncoder coreUpdateFn updateLocalFn msg model =
    case msg of
        LocalOrigin { localMsg, proposedEvent } ->
            let
                ( newLocalModel, localCmd ) =
                    Maybe.map (\l -> updateLocalFn l model.localModel) localMsg
                        |> Maybe.withDefault ( model.localModel, Cmd.none )

                ( newSharedModelInfo, sharedCmd ) =
                    Maybe.map (\s -> updateSharedLocalOrigin sharedModelMsgEncoder s model.sharedModelInfo) proposedEvent
                        |> Maybe.withDefault ( model.sharedModelInfo, Cmd.none )
            in
            ( { sharedModelInfo = newSharedModelInfo, localModel = newLocalModel }
            , Cmd.batch [ Cmd.map (localAction >> LocalOrigin) localCmd, sharedCmd ]
            )

        RemoteOrigin events ->
            ( { model
                | sharedModelInfo =
                    List.foldl (updateSharedRemoteModelOrigin coreUpdateFn) model.sharedModelInfo events
              }
            , Cmd.none
            )

        ControlMsg controlMsg ->
            let
                ( sharedModelInfo, cmd ) =
                    updateWithControlMsg sharedModelMsgEncoder coreUpdateFn controlMsg model.sharedModelInfo
            in
            ( { model | sharedModelInfo = sharedModelInfo }, cmd )


updateSharedLocalOrigin :
    (sharedModelMsg -> Json.Encode.Value)
    -> sharedModelMsg
    -> SharedModelInfo sharedModel sharedModelMsg
    -> ( SharedModelInfo sharedModel sharedModelMsg, Cmd (CloudMsg sharedModelMsg localModelMsg) )
updateSharedLocalOrigin sharedModelMsgEncoder msg model =
    let
        newEvent =
            { msg = msg, id = 0 }
    in
    case model.pendingEvents of
        [] ->
            ( { model | pendingEvents = [ newEvent ] }
            , proposeEvent sharedModelMsgEncoder msg model.latestKnownEventId 0
            )

        _ :: _ ->
            ( { model
                | pendingEvents = List.append model.pendingEvents [ newEvent ]
              }
            , Cmd.none
            )



-- TODO: Update this to be skeptical of whether the given event is really next


updateSharedRemoteModelOrigin :
    (sharedModelMsg -> sharedModel -> sharedModel)
    -> Event sharedModelMsg
    -> SharedModelInfo sharedModel sharedModelMsg
    -> SharedModelInfo sharedModel sharedModelMsg
updateSharedRemoteModelOrigin coreUpdateFn event model =
    { model
        | latestKnownSharedModel = coreUpdateFn event.msg model.latestKnownSharedModel
        , latestKnownEventId = event.id
    }


updateWithControlMsg :
    (sharedModelMsg -> Json.Encode.Value)
    -> (sharedModelMsg -> sharedModel -> sharedModel)
    -> ControlMsg sharedModelMsg
    -> SharedModelInfo sharedModel sharedModelMsg
    -> ( SharedModelInfo sharedModel sharedModelMsg, Cmd (CloudMsg sharedModelMsg localModelMsg) )
updateWithControlMsg sharedModelMsgEncoder coreUpdateFn controlMsg model =
    case controlMsg of
        Accept eventId clientEventId ->
            case model.pendingEvents of
                acceptedEvent :: pendingEvents ->
                    ( { model
                        | latestKnownEventId = eventId
                        , latestKnownSharedModel = coreUpdateFn acceptedEvent.msg model.latestKnownSharedModel
                        , pendingEvents = pendingEvents
                      }
                    , List.head pendingEvents
                        |> Maybe.map (\nextEventToSend -> proposeEvent sharedModelMsgEncoder nextEventToSend.msg eventId nextEventToSend.id)
                        |> Maybe.withDefault Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Reject clientId newerEvents ->
            let
                latest =
                    List.head (List.reverse newerEvents)
            in
            ( { model
                | latestKnownEventId = Maybe.map .id latest |> Maybe.withDefault model.latestKnownEventId
                , latestKnownSharedModel = List.foldl coreUpdateFn model.latestKnownSharedModel (List.map .msg newerEvents)
                , pendingEvents = []
              }
            , Cmd.none
            )


predictedSharedModel : (sharedModelMsg -> sharedModel -> sharedModel) -> SharedModelInfo sharedModel sharedModelMsg -> sharedModel
predictedSharedModel coreUpdateFn model =
    List.foldl coreUpdateFn model.latestKnownSharedModel (List.map .msg model.pendingEvents)


proposeEvent :
    (sharedModelMsg -> Json.Encode.Value)
    -> sharedModelMsg
    -> EventId
    -> ClientEventId
    -> Cmd (CloudMsg sharedModelMsg localModelMsg)
proposeEvent sharedModelMsgEncoder sharedModelMsg latestKnownEventId clientEventId =
    proposal
        (Json.Encode.object
            [ ( "sharedModelMsg", sharedModelMsgEncoder sharedModelMsg )
            , ( "latestKnownEventId", Json.Encode.int latestKnownEventId )
            , ( "clientEventId", Json.Encode.int clientEventId )
            ]
        )



-- Chat's specific update messages and functions


type alias Msg =
    CloudMsg SharedChatMsg LocalChatMsg


type alias ChatAction =
    LocalOriginAction SharedChatMsg LocalChatMsg


type SharedChatMsg
    = AddChat String


type LocalChatMsg
    = ChangeDraft String
    | DisplayError String


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    constructCloudUpdate encodeSharedModelMsg updateChatShared updateChatLocal


updateChatLocal : LocalChatMsg -> LocalChatModel -> ( LocalChatModel, Cmd msg )
updateChatLocal msg model =
    case msg of
        ChangeDraft draft ->
            ( { model | draft = draft }, Cmd.none )

        DisplayError error ->
            ( { model | errorMessage = Just error }, Cmd.none )


updateChatShared : SharedChatMsg -> SharedChatModel -> SharedChatModel
updateChatShared msg model =
    case msg of
        AddChat newChat ->
            { model | chats = List.append model.chats [ newChat ] }


encodeSharedModelMsg : SharedChatMsg -> Json.Encode.Value
encodeSharedModelMsg sharedModelMsg =
    case sharedModelMsg of
        AddChat string ->
            Json.Encode.object
                [ ( "addChat", Json.Encode.string string )
                ]


port proposal : Json.Encode.Value -> Cmd msg


port proposalResponse : (Json.Decode.Value -> msg) -> Sub msg


port receiveEvents : (Json.Decode.Value -> msg) -> Sub msg



-- VIEW
-- generic library stuff


constructCloudView :
    (sharedModelMsg -> sharedModel -> sharedModel)
    -> (sharedModel -> localModel -> Html (LocalOriginAction sharedModelMsg localModelMsg))
    -> CloudModel sharedModel sharedModelMsg localModel
    -> Html (CloudMsg sharedModelMsg localModelMsg)
constructCloudView updateFn viewFn model =
    viewFn (predictedSharedModel updateFn model.sharedModelInfo) model.localModel
        |> Html.map LocalOrigin



-- Chat's specific view


view : Model -> Html Msg
view =
    constructCloudView updateChatShared chatView


chatView : SharedChatModel -> LocalChatModel -> Html ChatAction
chatView sharedModel localModel =
    div []
        [ ul [ id "messages" ] (List.map (\chat -> li [] [ text chat ]) sharedModel.chats)
        , div [ id "chatform" ]
            [ input [ value localModel.draft, onInput (ChangeDraft >> localAction) ] []
            , button
                [ onClick { localMsg = Just (ChangeDraft ""), proposedEvent = Just (AddChat localModel.draft) } ]
                [ text "Send" ]
            ]
        ]
