module CloudModel exposing (CloudModelConfig, LocalOriginAction, element, localAction, sharedAction)

import Browser
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type alias CloudModelConfig sharedModel localModel sharedModelMsg localModelMsg flags =
    { sharedModelMsgDecoder : Json.Decode.Decoder sharedModelMsg
    , sharedModelMsgEncoder : sharedModelMsg -> Json.Decode.Value
    , displayError : String -> localModelMsg
    , init : flags -> ( sharedModel, localModel, Cmd localModelMsg )
    , updateCloud : sharedModelMsg -> sharedModel -> sharedModel
    , updateLocal : localModelMsg -> localModel -> ( localModel, Cmd localModelMsg )
    , subscriptions : sharedModel -> localModel -> Sub localModelMsg
    , view : sharedModel -> localModel -> Html (LocalOriginAction sharedModelMsg localModelMsg)
    , proposal : Json.Encode.Value -> Cmd (CloudMsg sharedModelMsg localModelMsg)
    , proposalResponse : (Json.Decode.Value -> CloudMsg sharedModelMsg localModelMsg) -> Sub (CloudMsg sharedModelMsg localModelMsg)
    , receiveEvents : (Json.Decode.Value -> CloudMsg sharedModelMsg localModelMsg) -> Sub (CloudMsg sharedModelMsg localModelMsg)
    }


element :
    CloudModelConfig sharedModel localModel sharedModelMsg localModelMsg flags
    -> Program flags (CloudModel sharedModel sharedModelMsg localModel) (CloudMsg sharedModelMsg localModelMsg)
element { sharedModelMsgDecoder, sharedModelMsgEncoder, displayError, init, updateCloud, updateLocal, subscriptions, view, proposal, proposalResponse, receiveEvents } =
    Browser.element
        { init = buildInit init
        , update =
            constructCloudUpdate proposal
                sharedModelMsgEncoder
                updateCloud
                updateLocal
        , subscriptions = buildSubscriptions proposalResponse receiveEvents sharedModelMsgDecoder displayError subscriptions
        , view = constructCloudView updateCloud view
        }



-- MODEL


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



-- INIT


buildInit :
    (flags -> ( sharedModel, localModel, Cmd localModelMsg ))
    -> flags
    -> ( CloudModel sharedModel sharedModelMsg localModel, Cmd (CloudMsg sharedModelMsg localModelMsg) )
buildInit clientInit flags =
    let
        ( initialClientSharedModel, initialClientLocalModel, cmd ) =
            clientInit flags
    in
    ( { localModel = initialClientLocalModel
      , sharedModelInfo = initSharedModelInfo initialClientSharedModel
      }
    , Cmd.map (LocalOrigin << localAction) cmd
    )


initSharedModelInfo : sharedModel -> SharedModelInfo sharedModel sharedModelMsg
initSharedModelInfo initLatestKnownSharedModel =
    { latestKnownEventId = 0
    , latestKnownSharedModel = initLatestKnownSharedModel
    , pendingEvents = []
    }



-- UPDATE


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


constructCloudUpdate :
    (Json.Encode.Value -> Cmd (CloudMsg sharedModelMsg localModelMsg))
    -> (sharedModelMsg -> Json.Encode.Value)
    -> (sharedModelMsg -> sharedModel -> sharedModel)
    -> (localModelMsg -> localModel -> ( localModel, Cmd localModelMsg ))
    -> CloudMsg sharedModelMsg localModelMsg
    -> CloudModel sharedModel sharedModelMsg localModel
    -> ( CloudModel sharedModel sharedModelMsg localModel, Cmd (CloudMsg sharedModelMsg localModelMsg) )
constructCloudUpdate proposal sharedModelMsgEncoder coreUpdateFn updateLocalFn msg model =
    case msg of
        LocalOrigin { localMsg, proposedEvent } ->
            let
                ( newLocalModel, localCmd ) =
                    Maybe.map (\l -> updateLocalFn l model.localModel) localMsg
                        |> Maybe.withDefault ( model.localModel, Cmd.none )

                ( newSharedModelInfo, sharedCmd ) =
                    Maybe.map (\s -> updateSharedLocalOrigin proposal sharedModelMsgEncoder s model.sharedModelInfo) proposedEvent
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
                    updateWithControlMsg proposal sharedModelMsgEncoder coreUpdateFn controlMsg model.sharedModelInfo
            in
            ( { model | sharedModelInfo = sharedModelInfo }, cmd )


updateSharedLocalOrigin :
    (Json.Encode.Value -> Cmd (CloudMsg sharedModelMsg localModelMsg))
    -> (sharedModelMsg -> Json.Encode.Value)
    -> sharedModelMsg
    -> SharedModelInfo sharedModel sharedModelMsg
    -> ( SharedModelInfo sharedModel sharedModelMsg, Cmd (CloudMsg sharedModelMsg localModelMsg) )
updateSharedLocalOrigin proposal sharedModelMsgEncoder msg model =
    let
        newEvent =
            { msg = msg, id = 0 }
    in
    case model.pendingEvents of
        [] ->
            ( { model | pendingEvents = [ newEvent ] }
            , proposeEvent proposal sharedModelMsgEncoder msg model.latestKnownEventId 0
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
    (Json.Encode.Value -> Cmd (CloudMsg sharedModelMsg localModelMsg))
    -> (sharedModelMsg -> Json.Encode.Value)
    -> (sharedModelMsg -> sharedModel -> sharedModel)
    -> ControlMsg sharedModelMsg
    -> SharedModelInfo sharedModel sharedModelMsg
    -> ( SharedModelInfo sharedModel sharedModelMsg, Cmd (CloudMsg sharedModelMsg localModelMsg) )
updateWithControlMsg proposal sharedModelMsgEncoder coreUpdateFn controlMsg model =
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
                        |> Maybe.map (\nextEventToSend -> proposeEvent proposal sharedModelMsgEncoder nextEventToSend.msg eventId nextEventToSend.id)
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
    (Json.Encode.Value -> Cmd (CloudMsg sharedModelMsg localModelMsg))
    -> (sharedModelMsg -> Json.Encode.Value)
    -> sharedModelMsg
    -> EventId
    -> ClientEventId
    -> Cmd (CloudMsg sharedModelMsg localModelMsg)
proposeEvent proposal sharedModelMsgEncoder sharedModelMsg latestKnownEventId clientEventId =
    proposal
        (Json.Encode.object
            [ ( "sharedModelMsg", sharedModelMsgEncoder sharedModelMsg )
            , ( "latestKnownEventId", Json.Encode.int latestKnownEventId )
            , ( "clientEventId", Json.Encode.int clientEventId )
            ]
        )



-- SUBSCRIPTIONS


buildSubscriptions :
    ((Json.Decode.Value -> CloudMsg sharedModelMsg localModelMsg) -> Sub (CloudMsg sharedModelMsg localModelMsg))
    -> ((Json.Decode.Value -> CloudMsg sharedModelMsg localModelMsg) -> Sub (CloudMsg sharedModelMsg localModelMsg))
    -> Json.Decode.Decoder sharedModelMsg
    -> (String -> localModelMsg)
    -> (sharedModel -> localModel -> Sub localModelMsg)
    -> CloudModel sharedModel sharedModelMsg localModel
    -> Sub (CloudMsg sharedModelMsg localModelMsg)
buildSubscriptions proposalResponse receiveEvents sharedMsgDecoder displayErrorFn subscriptionFn { sharedModelInfo, localModel } =
    let
        localSubscriptions =
            subscriptionFn sharedModelInfo.latestKnownSharedModel localModel

        handleDecodeFailure errorMessage =
            LocalOrigin (localAction (displayErrorFn errorMessage))
    in
    Sub.batch
        [ proposalResponse (decodeOrFail handleDecodeFailure (Json.Decode.map ControlMsg (decodeProposalResponse sharedMsgDecoder)))
        , receiveEvents (decodeOrFail handleDecodeFailure (Json.Decode.map RemoteOrigin (Json.Decode.list (decodeEvent sharedMsgDecoder))))
        , Sub.map (LocalOrigin << localAction) localSubscriptions
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


decodeOrFail : (String -> msg) -> Decoder msg -> Json.Decode.Value -> msg
decodeOrFail onError decoder json =
    case Json.Decode.decodeValue decoder json of
        Ok msg ->
            msg

        Err error ->
            onError (Json.Decode.errorToString error)



-- VIEW


constructCloudView :
    (sharedModelMsg -> sharedModel -> sharedModel)
    -> (sharedModel -> localModel -> Html (LocalOriginAction sharedModelMsg localModelMsg))
    -> CloudModel sharedModel sharedModelMsg localModel
    -> Html (CloudMsg sharedModelMsg localModelMsg)
constructCloudView updateFn viewFn model =
    viewFn (predictedSharedModel updateFn model.sharedModelInfo) model.localModel
        |> Html.map LocalOrigin



-- UTILITY


localAction : localModelMsg -> LocalOriginAction sharedModelMsg localModelMsg
localAction localMsg =
    { localMsg = Just localMsg, proposedEvent = Nothing }


sharedAction : sharedModelMsg -> LocalOriginAction sharedModelMsg localModelMsg
sharedAction sharedModelMsg =
    { localMsg = Nothing, proposedEvent = Just sharedModelMsg }
