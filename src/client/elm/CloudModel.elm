module CloudModel exposing
    ( CloudModelConfig, element, wrapAll
    , localAction, sharedAction
    , CloudModel, CloudMsg, LocalOriginAction
    )

{-| This module provides an abstraction for "shared-workspace" web applications where multiple
clients are conceptually manipulating a single, shared, persistent model of the world. Models are
synced across clients, but clients can continue to operate in offline mode with shared-model actions
taking effect immediately locally and reconciled with the rest of the clients when possible.

The key concept is that instead of having a single model and message type, `CloudModel` programs
have two of each: a shared model and corresponding shared messages to manipulate it that all clients
can see, and a local model and corresponding local messages that are private to each client.
To use the library, you need to provide an initial value for each of these models, an update function
for each type of message and its corresponding model, encoders and decoders for your messages so that
the library can handle syncing things with other clients, and some ports to communicate over.

In addition to building your Elm program out of the functions in this library, to get a CloudModel
program working you'll also need to use
[the associated `index.js`](https://github.com/matthewsj/cloudmodel/blob/master/src/client/index.js)
or another Javascript layer to communicate messages out of the client, and run
[the associated server](https://github.com/matthewsj/cloudmodel/blob/master/src/server/index.js) or
another similar server to manage communicating across all clients.


# Creating a CloudModel program

@docs CloudModelConfig, element, wrapAll


# Convenience

@docs localAction, sharedAction

-}

import Browser
import Html exposing (Html)
import Json.Decode exposing (Decoder, field, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


{-| Configuration for a cloud model based application. Where normal Elm applications are built around
a `Model` type and a `Msg` type, cloud model based applications have both a `SharedModel` and a
`LocalModel`, and corresponding `SharedMsg` and `LocalMsg`s that interact with them.
-}
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


{-| The equivalent of `Browser.element` for a `CloudModel`-based program. You should probably use
this as your program's main entry point.
-}
element :
    CloudModelConfig sharedModel localModel sharedMsg localMsg flags
    -> Program flags (CloudModel sharedModel sharedMsg localModel) (CloudMsg sharedMsg localMsg)
element cloudModelConfig =
    Browser.element (wrapAll cloudModelConfig)


{-| For users with special needs, for instance for composing multiple large framework libraries,
`wrapAll` takes a `CloudModel` config and produces standard Elm architecture functions.
-}
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
{- Internally the library works by wrapping user-supplied functions to produce new
   update/view/init functions that have `CloudModel` and `CloudMsg` types
   parameterized by user-supplied program-specific types. The extra functionality
   keeps track of what messages have been
-}


{-| The server's canonical ID for an event.
-}
type alias EventId =
    Int


{-| The client's non-canonical ID for an event.
-}
type alias ClientEventId =
    Int


{-| The model type used by wrapped functions.
-}
type alias CloudModel sharedModel sharedMsg localModel =
    { sharedModelInfo : SharedModelState sharedModel sharedMsg
    , localModel : localModel
    }


{-| A tracker that keeps track of the local client's understanding of the
shared model (known to be accurate at `lastKnownEventId`) plus some pending
messages that this client has proposed but that have not been accepted yet.
-}
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


initSharedModelState : sharedModel -> SharedModelState sharedModel sharedMsg
initSharedModelState initLatestKnownSharedModel =
    { latestKnownEventId = 0
    , latestKnownSharedModel = initLatestKnownSharedModel
    , pendingEvents = []
    }



-- UPDATE


{-| A message received from the server. Events are the canonical record of
what has happened to the cloud model and the client must defer to them.
-}
type alias Event sharedMsg =
    { msg : sharedMsg
    , id : EventId
    }


{-| An action that originated on the local client. This may contain a local
message intended to be consumed by the local client only, and may also
contain a shared message that this client is proposing be applied to the
shared model.
-}
type alias LocalOriginAction sharedMsg localMsg =
    { localMsg : Maybe localMsg
    , proposedEvent : Maybe sharedMsg
    }


{-| A message coming from the server to the client in response to one of
our proposals. If an event is accepted it is incorporated into the
canonical model; if rejected we learn the rejected ID and also the complete
list of events that happened after the last event our client knows about
at the time of rejection.
-}
type ControlMsg sharedMsg
    = Accept EventId ClientEventId
    | Reject ClientEventId (List (Event sharedMsg))


{-| The overall message type this program will use, which can be either a
local message, a remote message to be applied to our shared model, or a
control message in response to one of our proposals.
-}
type CloudMsg sharedMsg localMsg
    = LocalOrigin (LocalOriginAction sharedMsg localMsg)
    | RemoteOrigin (List (Event sharedMsg))
    | ControlMsg (ControlMsg sharedMsg)


{-| Wraps the user-provided cloud update function. This function requires
four parameters: the cloud and local update functions, and also a port
for sending proposals and an encoder for cloud messages. It returns an update
function.
-}
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


{-| Updates the model to take into account a new, locally-proposed shared message.
The shared message applies to this client's view of the world immediately but may
be rolled back if rejected by the server.
-}
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


{-| Updates the shared model with a new message received from the server.
These messages are always canonical.
-}
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


{-| Updates the shared model with the result of a server-side control message.
-}
updateWithControlMsg :
    (Json.Encode.Value -> Cmd (CloudMsg sharedMsg localMsg))
    -> (sharedMsg -> Json.Encode.Value)
    -> (sharedMsg -> sharedModel -> sharedModel)
    -> ControlMsg sharedMsg
    -> SharedModelState sharedModel sharedMsg
    -> ( SharedModelState sharedModel sharedMsg, Cmd (CloudMsg sharedMsg localMsg) )
updateWithControlMsg proposal sharedMsgEncoder coreUpdateFn controlMsg model =
    case controlMsg of
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


{-| Constructs a projected version of the shared model assuming all
of our proposed events are accepted. This allows the local client
to keep operating even if the server is temporarily unresponsive
by predicting the results of actions.
-}
predictedSharedModel :
    (sharedMsg -> sharedModel -> sharedModel)
    -> SharedModelState sharedModel sharedMsg
    -> sharedModel
predictedSharedModel coreUpdateFn model =
    List.foldl coreUpdateFn model.latestKnownSharedModel (List.map .msg model.pendingEvents)


{-| Handles building a command that sends a JSON representation of
a proposed action to the given port, which can then communicate it
to the server for consideration.
-}
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


{-| Convenience function that constructs a `LocalOriginAction`
that only takes a local action.
-}
localAction : localMsg -> LocalOriginAction sharedMsg localMsg
localAction localMsg =
    { localMsg = Just localMsg, proposedEvent = Nothing }


{-| Convenience function that constructs a `LocalOriginAction`
that only takes a shared action.
-}
sharedAction : sharedMsg -> LocalOriginAction sharedMsg localMsg
sharedAction sharedMsg =
    { localMsg = Nothing, proposedEvent = Just sharedMsg }
