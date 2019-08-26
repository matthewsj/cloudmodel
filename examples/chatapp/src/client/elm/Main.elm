port module Main exposing (main)

import CloudModel exposing (localAction, RejectionStrategy(..))
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode



-- MAIN


main =
    CloudModel.element
        { sharedMsgDecoder = decodeSharedMsg
        , sharedMsgEncoder = encodeSharedMsg
        , onDecodeError = DecodeError
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


{-| The model that is shared across all clients.
-}
type alias SharedModel =
    { chats : List String
    }


{-| The model that is local to this individual client.
-}
type alias LocalModel =
    { draft : String
    , errorMessage : Maybe String
    }



-- INIT


{-| Initial values for shared and local models as well as local commands to
run on start.
-}
init : () -> ( SharedModel, LocalModel, Cmd LocalChatMsg )
init flags =
    ( initSharedModel
    , initLocalModel
    , Cmd.none
    )


{-| The initial shared model.
-}
initSharedModel : SharedModel
initSharedModel =
    { chats = []
    }


{-| The initial local model.
-}
initLocalModel : LocalModel
initLocalModel =
    { draft = ""
    , errorMessage = Nothing
    }



-- SHARED UPDATE


{-| Shared messages. These messages update the shared model and we must write
encoders and decoders for them so that they can be communicated over the
network.
-}
type SharedMsg
    = AddChat String


{-| Encodes shared messages to JSON.
-}
encodeSharedMsg : SharedMsg -> Json.Encode.Value
encodeSharedMsg sharedModelMsg =
    case sharedModelMsg of
        AddChat string ->
            Json.Encode.object
                [ ( "addChat", Json.Encode.string string )
                ]


{-| Decodes shared messages from the JSON representation written by
`encodeSharedMsg`.
-}
decodeSharedMsg : Decoder SharedMsg
decodeSharedMsg =
    oneOf
        [ succeed AddChat |> required "addChat" Json.Decode.string
        ]


{-| Updates the shared model. Shared model updates are not allowed to issue
commands.
-}
updateShared : SharedMsg -> SharedModel -> SharedModel
updateShared msg model =
    case msg of
        AddChat newChat ->
            { model | chats = List.append model.chats [ newChat ] }



-- LOCAL UPDATE


{-| Local messages. These messages are applied to the local model only and may
issue commands.
-}
type LocalChatMsg
    = ChangeDraft String
    | DecodeError String


updateLocal : LocalChatMsg -> LocalModel -> ( LocalModel, Cmd msg )
updateLocal msg model =
    case msg of
        ChangeDraft draft ->
            ( { model | draft = draft }, Cmd.none )

        DecodeError error ->
            ( { model | errorMessage = Just error }, Cmd.none )


type alias ChatAction =
    CloudModel.LocalOriginAction SharedMsg LocalChatMsg



-- VIEW


view : SharedModel -> LocalModel -> Html ChatAction
view sharedModel localModel =
    div []
        [ div [id "errorMessage"] [ text (Maybe.withDefault "" localModel.errorMessage) ]
        , ul [ id "messages" ] (List.map (\chat -> li [] [ text chat ]) sharedModel.chats)
        , div [ id "chatform" ]
            [ input [ value localModel.draft, onInput (ChangeDraft >> localAction) ] []
            , button
                [ onClick { localMsg = Just (ChangeDraft ""), proposedEvent = Just (AddChat localModel.draft) } ]
                [ text "Send" ]
            ]
        ]


port proposal : Json.Encode.Value -> Cmd msg


port proposalResponse : (Json.Decode.Value -> msg) -> Sub msg


port receiveEvents : (Json.Decode.Value -> msg) -> Sub msg
