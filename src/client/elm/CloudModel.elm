module CloudModel exposing (CloudModel, CloudModelConfig, CloudMsg, LocalOriginAction, element, localAction, sharedAction, wrapAll)

import Browser
import Html exposing (Html)
import Json.Decode exposing (Decoder, field, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type alias CloudModelConfig sharedModel localModel sharedMsg localMsg flags =
    { sharedMsgDecoder : Json.Decode.Decoder sharedMsg
    , sharedMsgEncoder : sharedMsg -> Json.Decode.Value
    , displayError : String -> localMsg
    , init : flags -> ( sharedModel, localModel, Cmd localMsg )
    , updateCloud : sharedMsg -> sharedModel -> sharedModel
    , updateLocal : localMsg -> localModel -> ( localModel, Cmd localMsg )
    , subscriptions : sharedModel -> localModel -> Sub localMsg
    , view : sharedModel -> localModel -> Html (LocalOriginAction sharedMsg localMsg)
    , proposal : Json.Encode.Value -> Cmd (CloudMsg sharedMsg localMsg)
    , proposalResponse :
        (Json.Decode.Value -> CloudMsg sharedMsg localMsg)
        -> Sub (CloudMsg sharedMsg localMsg)
    , receiveEvents :
        (Json.Decode.Value -> CloudMsg sharedMsg localMsg)
        -> Sub (CloudMsg sharedMsg localMsg)
    }


element :
    CloudModelConfig sharedModel localModel sharedMsg localMsg flags
    -> Program flags (CloudModel sharedModel sharedMsg localModel) (CloudMsg sharedMsg localMsg)
element cloudModelConfig =
    Browser.element (wrapAll cloudModelConfig)


wrapAll :
    CloudModelConfig sharedModel localModel sharedMsg localMsg flags
    ->
        { init :
            flags
            -> ( CloudModel sharedModel sharedMsg localModel, Cmd (CloudMsg sharedMsg localMsg) )
        , update :
            CloudMsg sharedMsg localMsg
            -> CloudModel sharedModel sharedMsg localModel
            -> ( CloudModel sharedModel sharedMsg localModel, Cmd (CloudMsg sharedMsg localMsg) )
        , subscriptions :
            CloudModel sharedModel sharedMsg localModel
            -> Sub (CloudMsg sharedMsg localMsg)
        , view : CloudModel sharedModel sharedMsg localModel -> Html (CloudMsg sharedMsg localMsg)
        }
wrapAll { sharedMsgDecoder, sharedMsgEncoder, displayError, init, updateCloud, updateLocal, subscriptions, view, proposal, proposalResponse, receiveEvents } =
    { init = buildInit init
    , update =
        buildCloudUpdate proposal
            sharedMsgEncoder
            updateCloud
            updateLocal
    , subscriptions =
        buildSubscriptions proposalResponse
            receiveEvents
            sharedMsgDecoder
            displayError
            subscriptions
    , view = constructCloudView updateCloud view
    }



-- MODEL


type alias EventId =
    Int


type alias ClientEventId =
    Int


type alias CloudModel sharedModel sharedMsg localModel =
    { sharedModelInfo : SharedModelState sharedModel sharedMsg
    , localModel : localModel
    }


type alias SharedModelState sharedModel sharedMsg =
    { latestKnownEventId : EventId
    , latestKnownSharedModel : sharedModel
    , pendingEvents : List (Event sharedMsg)
    }



-- INIT


buildInit :
    (flags -> ( sharedModel, localModel, Cmd localMsg ))
    -> flags
    -> ( CloudModel sharedModel sharedMsg localModel, Cmd (CloudMsg sharedMsg localMsg) )
buildInit clientInit flags =
    let
        ( initialClientSharedModel, initialClientLocalModel, cmd ) =
            clientInit flags
    in
    ( { localModel = initialClientLocalModel
      , sharedModelInfo = initSharedModelState initialClientSharedModel
      }
    , Cmd.map (LocalOrigin << localAction) cmd
    )


-- Q (YK 2019/03/15): Should this also take a latest known event id or will the sharedModel always be empty?
initSharedModelState : sharedModel -> SharedModelState sharedModel sharedMsg
initSharedModelState initLatestKnownSharedModel =
    { latestKnownEventId = 0
    , latestKnownSharedModel = initLatestKnownSharedModel
    , pendingEvents = []
    }



-- UPDATE


type alias Event sharedMsg =
    { msg : sharedMsg
    , id : EventId
    }


type alias LocalOriginAction sharedMsg localMsg =
    { localMsg : Maybe localMsg
    , proposedEvent : Maybe sharedMsg
    }


type CloudMsg sharedMsg localMsg
    = LocalOrigin (LocalOriginAction sharedMsg localMsg)
    | RemoteOrigin (List (Event sharedMsg))
    | ControlMsg (ControlMsg sharedMsg)


type ControlMsg sharedMsg
    = Accept EventId ClientEventId
    | Reject ClientEventId (List (Event sharedMsg))


buildCloudUpdate :
    (Json.Encode.Value -> Cmd (CloudMsg sharedMsg localMsg))
    -> (sharedMsg -> Json.Encode.Value)
    -> (sharedMsg -> sharedModel -> sharedModel)
    -> (localMsg -> localModel -> ( localModel, Cmd localMsg ))
    -> CloudMsg sharedMsg localMsg
    -> CloudModel sharedModel sharedMsg localModel
    -> ( CloudModel sharedModel sharedMsg localModel, Cmd (CloudMsg sharedMsg localMsg) )
buildCloudUpdate proposal sharedMsgEncoder coreUpdateFn updateLocalFn msg model =
    case msg of
        LocalOrigin { localMsg, proposedEvent } ->
            let
                ( newLocalModel, localCmd ) =
                    Maybe.map (\l -> updateLocalFn l model.localModel) localMsg
                        |> Maybe.withDefault ( model.localModel, Cmd.none )

                ( newSharedModelState, sharedCmd ) =
                    Maybe.map
                        (\s ->
                            updateSharedLocalOrigin proposal
                                sharedMsgEncoder
                                s
                                model.sharedModelInfo
                        )
                        proposedEvent
                        |> Maybe.withDefault ( model.sharedModelInfo, Cmd.none )
            in
            ( { sharedModelInfo = newSharedModelState, localModel = newLocalModel }
            , Cmd.batch [ Cmd.map (localAction >> LocalOrigin) localCmd, sharedCmd ]
            )

        RemoteOrigin events ->
            ( { model
                | sharedModelInfo =
                    List.foldl (updateSharedRemoteModelOrigin coreUpdateFn)
                        model.sharedModelInfo
                        events
              }
            , Cmd.none
            )

        ControlMsg controlMsg ->
            let
                ( sharedModelInfo, cmd ) =
                    updateWithControlMsg proposal
                        sharedMsgEncoder
                        coreUpdateFn
                        controlMsg
                        model.sharedModelInfo
            in
            ( { model | sharedModelInfo = sharedModelInfo }, cmd )


updateSharedLocalOrigin :
    (Json.Encode.Value -> Cmd (CloudMsg sharedMsg localMsg))
    -> (sharedMsg -> Json.Encode.Value)
    -> sharedMsg
    -> SharedModelState sharedModel sharedMsg
    -> ( SharedModelState sharedModel sharedMsg, Cmd (CloudMsg sharedMsg localMsg) )
updateSharedLocalOrigin proposal sharedMsgEncoder msg model =
    let
        newEvent =
            { msg = msg, id = 0 }
    in
    case model.pendingEvents of
        [] ->
            ( { model | pendingEvents = [ newEvent ] }
            , proposeEvent proposal sharedMsgEncoder msg model.latestKnownEventId 0
            )

        _ :: _ ->
            ( { model
                | pendingEvents = List.append model.pendingEvents [ newEvent ]
              }
            , Cmd.none
            )



-- TODO: Update this to be skeptical of whether the given event is really next


updateSharedRemoteModelOrigin :
    (sharedMsg -> sharedModel -> sharedModel)
    -> Event sharedMsg
    -> SharedModelState sharedModel sharedMsg
    -> SharedModelState sharedModel sharedMsg
updateSharedRemoteModelOrigin coreUpdateFn event model =
    { model
        | latestKnownSharedModel = coreUpdateFn event.msg model.latestKnownSharedModel
        , latestKnownEventId = event.id
    }


updateWithControlMsg :
    (Json.Encode.Value -> Cmd (CloudMsg sharedMsg localMsg))
    -> (sharedMsg -> Json.Encode.Value)
    -> (sharedMsg -> sharedModel -> sharedModel)
    -> ControlMsg sharedMsg
    -> SharedModelState sharedModel sharedMsg
    -> ( SharedModelState sharedModel sharedMsg, Cmd (CloudMsg sharedMsg localMsg) )
updateWithControlMsg proposal sharedMsgEncoder coreUpdateFn controlMsg model =
    case controlMsg of
        -- Q (YK 2019/03/15): It looks like `clientEventId` is unused?
        Accept eventId clientEventId ->
            case model.pendingEvents of
                acceptedEvent :: pendingEvents ->
                    ( { model
                        | latestKnownEventId = eventId
                        , latestKnownSharedModel =
                            coreUpdateFn acceptedEvent.msg
                                model.latestKnownSharedModel
                        , pendingEvents = pendingEvents
                      }
                    , List.head pendingEvents
                        |> Maybe.map
                            (\nextEventToSend ->
                                proposeEvent proposal
                                    sharedMsgEncoder
                                    nextEventToSend.msg
                                    eventId
                                    nextEventToSend.id
                            )
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
                | latestKnownEventId =
                    Maybe.map .id latest
                        |> Maybe.withDefault model.latestKnownEventId
                , latestKnownSharedModel =
                    List.foldl coreUpdateFn
                        model.latestKnownSharedModel
                        (List.map .msg newerEvents)
                , pendingEvents = []
              }
            , Cmd.none
            )


predictedSharedModel :
    (sharedMsg -> sharedModel -> sharedModel)
    -> SharedModelState sharedModel sharedMsg
    -> sharedModel
predictedSharedModel coreUpdateFn model =
    List.foldl coreUpdateFn model.latestKnownSharedModel (List.map .msg model.pendingEvents)


proposeEvent :
    (Json.Encode.Value -> Cmd (CloudMsg sharedMsg localMsg))
    -> (sharedMsg -> Json.Encode.Value)
    -> sharedMsg
    -> EventId
    -> ClientEventId
    -> Cmd (CloudMsg sharedMsg localMsg)
proposeEvent proposal sharedMsgEncoder sharedMsg latestKnownEventId clientEventId =
    proposal
        (Json.Encode.object
            [ ( "sharedMsg", sharedMsgEncoder sharedMsg )
            , ( "latestKnownEventId", Json.Encode.int latestKnownEventId )
            , ( "clientEventId", Json.Encode.int clientEventId )
            ]
        )



-- SUBSCRIPTIONS


buildSubscriptions :
    ((Json.Decode.Value -> CloudMsg sharedMsg localMsg) -> Sub (CloudMsg sharedMsg localMsg))
    -> ((Json.Decode.Value -> CloudMsg sharedMsg localMsg) -> Sub (CloudMsg sharedMsg localMsg))
    -> Json.Decode.Decoder sharedMsg
    -> (String -> localMsg)
    -> (sharedModel -> localModel -> Sub localMsg)
    -> CloudModel sharedModel sharedMsg localModel
    -> Sub (CloudMsg sharedMsg localMsg)
buildSubscriptions proposalResponse receiveEvents sharedMsgDecoder displayErrorFn subscriptionFn { sharedModelInfo, localModel } =
    let
        localSubscriptions =
            subscriptionFn sharedModelInfo.latestKnownSharedModel localModel

        handleDecodeFailure errorMessage =
            LocalOrigin (localAction (displayErrorFn errorMessage))
    in
    Sub.batch
        [ proposalResponse
            (decodeOrFail handleDecodeFailure
                (Json.Decode.map ControlMsg (decodeProposalResponse sharedMsgDecoder))
            )
        , receiveEvents
            (decodeOrFail handleDecodeFailure
                (Json.Decode.map RemoteOrigin (Json.Decode.list (decodeEvent sharedMsgDecoder)))
            )
        , Sub.map (LocalOrigin << localAction) localSubscriptions
        ]


decodeProposalResponse : Decoder sharedMsg -> Decoder (ControlMsg sharedMsg)
decodeProposalResponse decodeSharedMsg =
    oneOf
        [ field "accept" decodeAccept
        , field "reject" (decodeReject decodeSharedMsg)
        ]


decodeAccept : Decoder (ControlMsg sharedMsg)
decodeAccept =
    succeed Accept
        |> required "eventId" Json.Decode.int
        |> required "clientEventId" Json.Decode.int


decodeReject : Decoder sharedMsg -> Decoder (ControlMsg sharedMsg)
decodeReject decodeSharedMsg =
    succeed Reject
        |> required "clientEventId" Json.Decode.int
        |> required "missingEvents" (Json.Decode.list (decodeEvent decodeSharedMsg))


decodeEvent : Decoder sharedMsg -> Decoder (Event sharedMsg)
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
    (sharedMsg -> sharedModel -> sharedModel)
    -> (sharedModel -> localModel -> Html (LocalOriginAction sharedMsg localMsg))
    -> CloudModel sharedModel sharedMsg localModel
    -> Html (CloudMsg sharedMsg localMsg)
constructCloudView updateFn viewFn model =
    viewFn (predictedSharedModel updateFn model.sharedModelInfo) model.localModel
        |> Html.map LocalOrigin



-- UTILITY


localAction : localMsg -> LocalOriginAction sharedMsg localMsg
localAction localMsg =
    { localMsg = Just localMsg, proposedEvent = Nothing }


sharedAction : sharedMsg -> LocalOriginAction sharedMsg localMsg
sharedAction sharedMsg =
    { localMsg = Nothing, proposedEvent = Just sharedMsg }
