port module Main exposing (main)

import CloudModel exposing (localAction)
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode



-- MAIN


main =
    CloudModel.element
        { sharedModelMsgDecoder = decodeSharedMsg
        , sharedModelMsgEncoder = encodeSharedMsg
        , displayError = DisplayError
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


type alias SharedModel =
    { chats : List String
    }


type alias LocalModel =
    { draft : String
    , errorMessage : Maybe String
    }



-- INIT


init : () -> ( SharedModel, LocalModel, Cmd LocalChatMsg )
init flags =
    ( initSharedModel
    , initLocalModel
    , Cmd.none
    )


initSharedModel : SharedModel
initSharedModel =
    { chats = []
    }


initLocalModel : LocalModel
initLocalModel =
    { draft = ""
    , errorMessage = Nothing
    }



-- SHARED UPDATE


type SharedMsg
    = AddChat String


decodeSharedMsg : Decoder SharedMsg
decodeSharedMsg =
    oneOf
        [ succeed AddChat |> required "addChat" Json.Decode.string
        ]


encodeSharedMsg : SharedMsg -> Json.Encode.Value
encodeSharedMsg sharedModelMsg =
    case sharedModelMsg of
        AddChat string ->
            Json.Encode.object
                [ ( "addChat", Json.Encode.string string )
                ]


updateShared : SharedMsg -> SharedModel -> SharedModel
updateShared msg model =
    case msg of
        AddChat newChat ->
            { model | chats = List.append model.chats [ newChat ] }



-- LOCAL UPDATE


type LocalChatMsg
    = ChangeDraft String
    | DisplayError String


updateLocal : LocalChatMsg -> LocalModel -> ( LocalModel, Cmd msg )
updateLocal msg model =
    case msg of
        ChangeDraft draft ->
            ( { model | draft = draft }, Cmd.none )

        DisplayError error ->
            ( { model | errorMessage = Just error }, Cmd.none )


type alias ChatAction =
    CloudModel.LocalOriginAction SharedMsg LocalChatMsg



-- VIEW


view : SharedModel -> LocalModel -> Html ChatAction
view sharedModel localModel =
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
