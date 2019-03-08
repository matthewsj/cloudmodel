port module Main exposing (main)

import Browser
import CloudModel exposing (localAction)
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode



-- MAIN


main =
    CloudModel.element
        { sharedModelMsgDecoder = decodeSharedModelMsg
        , sharedModelMsgEncoder = encodeSharedModelMsg
        , displayError = DisplayError
        , init = chatInit
        , updateCloud = updateChatShared
        , updateLocal = updateChatLocal
        , subscriptions = \_ _ -> Sub.none
        , view = chatView
        , proposal = proposal
        , proposalResponse = proposalResponse
        , receiveEvents = receiveEvents
        }



-- SUBSCRIPTIONS


decodeSharedModelMsg : Decoder SharedChatMsg
decodeSharedModelMsg =
    Json.Decode.oneOf
        [ succeed AddChat |> required "addChat" Json.Decode.string
        ]



-- MODEL


type alias SharedChatModel =
    { chats : List String
    }


type alias LocalChatModel =
    { draft : String
    , errorMessage : Maybe String
    }



-- chat-specific init stuff


chatInit : () -> ( SharedChatModel, LocalChatModel, Cmd LocalChatMsg )
chatInit flags =
    ( initSharedChatModel
    , initLocalChatModel
    , Cmd.none
    )


initSharedChatModel : SharedChatModel
initSharedChatModel =
    { chats = []
    }


initLocalChatModel : LocalChatModel
initLocalChatModel =
    { draft = ""
    , errorMessage = Nothing
    }



-- UPDATE


type alias ChatAction =
    CloudModel.LocalOriginAction SharedChatMsg LocalChatMsg


type SharedChatMsg
    = AddChat String


type LocalChatMsg
    = ChangeDraft String
    | DisplayError String


updateChatShared : SharedChatMsg -> SharedChatModel -> SharedChatModel
updateChatShared msg model =
    case msg of
        AddChat newChat ->
            { model | chats = List.append model.chats [ newChat ] }


updateChatLocal : LocalChatMsg -> LocalChatModel -> ( LocalChatModel, Cmd msg )
updateChatLocal msg model =
    case msg of
        ChangeDraft draft ->
            ( { model | draft = draft }, Cmd.none )

        DisplayError error ->
            ( { model | errorMessage = Just error }, Cmd.none )


encodeSharedModelMsg : SharedChatMsg -> Json.Encode.Value
encodeSharedModelMsg sharedModelMsg =
    case sharedModelMsg of
        AddChat string ->
            Json.Encode.object
                [ ( "addChat", Json.Encode.string string )
                ]



-- VIEW


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


port proposal : Json.Encode.Value -> Cmd msg


port proposalResponse : (Json.Decode.Value -> msg) -> Sub msg


port receiveEvents : (Json.Decode.Value -> msg) -> Sub msg
