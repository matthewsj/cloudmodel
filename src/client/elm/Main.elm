port module Main exposing (ClientEventId, ControlMsg(..), Event, EventId, LocalModel, LocalModelMsg(..), Model, Msg(..), SharedModel, SharedModelMsg(..), coreUpdate, decodeAccept, decodeEvent, decodeOrFail, decodeProposalResponse, decodeReject, decodeSharedModelMsg, encodeSharedModelMsg, init, initLocalModel, initSharedModel, main, predictedSharedModel, proposal, proposalResponse, proposeEvent, receiveEvents, subscriptions, update, updateAll, updateLocal, updateSharedLocalOrigin, updateSharedRemoteModelOrigin, updateWithControlMsg, view, viewSharedAndLocal)

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
        [ proposalResponse (decodeOrFail (Json.Decode.map ControlMsg decodeProposalResponse))
        , receiveEvents (decodeOrFail (Json.Decode.map RemoteOrigin (Json.Decode.list decodeEvent)))
        ]


decodeProposalResponse : Decoder ControlMsg
decodeProposalResponse =
    oneOf
        [ field "accept" decodeAccept
        , field "reject" decodeReject
        ]


decodeAccept : Decoder ControlMsg
decodeAccept =
    succeed Accept
        |> required "eventId" Json.Decode.int
        |> required "clientEventId" Json.Decode.int


decodeReject : Decoder ControlMsg
decodeReject =
    succeed Reject
        |> required "clientEventId" Json.Decode.int
        |> required "missingEvents" (Json.Decode.list decodeEvent)


decodeEvent : Decoder Event
decodeEvent =
    succeed Event
        |> required "msg" decodeSharedModelMsg
        |> required "id" Json.Decode.int


decodeSharedModelMsg : Decoder SharedModelMsg
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


type alias EventId =
    Int


type alias ClientEventId =
    Int


type alias Model =
    { latestKnownEventId : EventId
    , latestKnownSharedModel : SharedModel
    , pendingEvents : List Event
    , localModel : LocalModel
    }


type alias SharedModel =
    { messages : List String
    }


type alias LocalModel =
    { draft : String
    , errorMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { latestKnownEventId = 0
      , latestKnownSharedModel = initSharedModel
      , pendingEvents = []
      , localModel = initLocalModel
      }
    , Cmd.none
    )


initSharedModel : SharedModel
initSharedModel =
    { messages = []
    }


initLocalModel : LocalModel
initLocalModel =
    { draft = ""
    , errorMessage = Nothing
    }



-- UPDATE


type alias Event =
    { msg : SharedModelMsg
    , id : EventId
    }


type alias LocalOriginAction =
    { localMsg : Maybe LocalModelMsg
    , proposedEvent : Maybe SharedModelMsg
    }


type Msg
    = LocalOrigin LocalOriginAction
    | RemoteOrigin (List Event)
    | ControlMsg ControlMsg


localAction : LocalModelMsg -> LocalOriginAction
localAction localMsg =
    { localMsg = Just localMsg, proposedEvent = Nothing }


sharedAction : SharedModelMsg -> LocalOriginAction
sharedAction sharedModelMsg =
    { localMsg = Nothing, proposedEvent = Just sharedModelMsg }


type SharedModelMsg
    = AddChat String


type LocalModelMsg
    = ChangeDraft String
    | DisplayError String


type ControlMsg
    = Accept EventId ClientEventId
    | Reject ClientEventId (List Event)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocalOrigin { localMsg, proposedEvent } ->
            let
                ( newLocalModel, localCmd ) =
                    Maybe.map (\l -> updateLocal l model.localModel) localMsg
                        |> Maybe.withDefault ( model.localModel, Cmd.none )

                ( newSharedModel, sharedCmd ) =
                    Maybe.map (\s -> updateSharedLocalOrigin s model) proposedEvent
                        |> Maybe.withDefault ( model, Cmd.none )
            in
            ( { newSharedModel | localModel = newLocalModel }
            , Cmd.batch [ localCmd, sharedCmd ]
            )

        RemoteOrigin events ->
            ( List.foldl updateSharedRemoteModelOrigin model events
            , Cmd.none
            )

        ControlMsg controlMsg ->
            updateWithControlMsg controlMsg model


updateLocal : LocalModelMsg -> LocalModel -> ( LocalModel, Cmd Msg )
updateLocal msg model =
    case msg of
        ChangeDraft draft ->
            ( { model | draft = draft }, Cmd.none )

        DisplayError error ->
            ( { model | errorMessage = Just error }, Cmd.none )


updateSharedLocalOrigin : SharedModelMsg -> Model -> ( Model, Cmd Msg )
updateSharedLocalOrigin msg model =
    let
        newEvent =
            { msg = msg, id = 0 }
    in
    case model.pendingEvents of
        [] ->
            ( { model | pendingEvents = [ newEvent ] }
            , proposeEvent msg model.latestKnownEventId 0
            )

        _ :: _ ->
            ( { model
                | pendingEvents = List.append model.pendingEvents [ newEvent ]
              }
            , Cmd.none
            )



-- TODO: Update this to be skeptical of whether the given event is really next


updateSharedRemoteModelOrigin : Event -> Model -> Model
updateSharedRemoteModelOrigin event model =
    { model
        | latestKnownSharedModel = coreUpdate event.msg model.latestKnownSharedModel
        , latestKnownEventId = event.id
    }


updateWithControlMsg : ControlMsg -> Model -> ( Model, Cmd Msg )
updateWithControlMsg controlMsg model =
    case controlMsg of
        Accept eventId clientEventId ->
            case model.pendingEvents of
                acceptedEvent :: pendingEvents ->
                    ( { model
                        | latestKnownEventId = eventId
                        , latestKnownSharedModel = coreUpdate acceptedEvent.msg model.latestKnownSharedModel
                        , pendingEvents = pendingEvents
                      }
                    , List.head pendingEvents
                        |> Maybe.map (\nextEventToSend -> proposeEvent nextEventToSend.msg eventId nextEventToSend.id)
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
                , latestKnownSharedModel = updateAll (List.map .msg newerEvents) model.latestKnownSharedModel
                , pendingEvents = []
              }
            , Cmd.none
            )


coreUpdate : SharedModelMsg -> SharedModel -> SharedModel
coreUpdate msg model =
    case msg of
        AddChat newChat ->
            { model | messages = List.append model.messages [ newChat ] }


predictedSharedModel : Model -> SharedModel
predictedSharedModel model =
    updateAll (List.map .msg model.pendingEvents) model.latestKnownSharedModel


updateAll : List SharedModelMsg -> SharedModel -> SharedModel
updateAll msgs model =
    List.foldl coreUpdate model msgs


proposeEvent : SharedModelMsg -> EventId -> ClientEventId -> Cmd Msg
proposeEvent sharedModelMsg latestKnownEventId clientEventId =
    proposal
        (Json.Encode.object
            [ ( "sharedModelMsg", encodeSharedModelMsg sharedModelMsg )
            , ( "latestKnownEventId", Json.Encode.int latestKnownEventId )
            , ( "clientEventId", Json.Encode.int clientEventId )
            ]
        )


encodeSharedModelMsg : SharedModelMsg -> Json.Encode.Value
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


view : Model -> Html Msg
view model =
    Html.map LocalOrigin (viewLocalOriginAction model)


viewLocalOriginAction : Model -> Html LocalOriginAction
viewLocalOriginAction model =
    viewSharedAndLocal (predictedSharedModel model) model.localModel


viewSharedAndLocal : SharedModel -> LocalModel -> Html LocalOriginAction
viewSharedAndLocal sharedModel localModel =
    div []
        [ ul [ id "messages" ] (List.map (\chat -> li [] [ text chat ]) sharedModel.messages)
        , div [ id "chatform" ]
            [ input [ value localModel.draft, onInput (ChangeDraft >> localAction) ] []
            , button
                [ onClick { localMsg = Just (ChangeDraft ""), proposedEvent = Just (AddChat localModel.draft) } ]
                [ text "Send" ]
            ]
        ]
