module Users exposing (..)

import Api.UserRegisterPayload exposing (UserRegisterPayload, userRegisterPayloadEncoder)
import Element exposing (..)
import Element.Events exposing (onClick)
import Flags exposing (Flags)
import Http
import UI.Button as Button
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField
import WithUuid exposing (encodeWithUuid)


type Msg
    = RegisterUserClicked
    | GotRegisterUserResult
    | EmailChanged String


type alias Model =
    { email : String, flags : Flags }


init : Flags -> Model
init flags =
    { email = "", flags = flags }


registerUser : Flags -> UserRegisterPayload -> Cmd Msg
registerUser flags userRegisterPayload =
    Http.post
        { url = flags.backendUrl ++ "/users"
        , body =
            userRegisterPayload
                |> encodeWithUuid userRegisterPayloadEncoder "550e8400-e29b-41d4-a726-446655440043"
                |> Http.jsonBody
        , expect = Http.expectWhatever (always GotRegisterUserResult)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterUserClicked ->
            ( model, registerUser model.flags { email = model.email } )

        GotRegisterUserResult ->
            ( model, Cmd.none )

        EmailChanged email ->
            ( { model | email = email }, Cmd.none )


view : RenderConfig -> Model -> Element Msg
view renderConfig model =
    column [ width fill, alignTop, padding 10, spacing 20, onClick <| RegisterUserClicked ]
        [ "Notepad"
            |> Text.heading1
            |> Text.withColor (Palette.color Palette.toneGray Palette.brightnessDarkest)
            |> Text.renderElement renderConfig
        , TextField.singlelineText EmailChanged "john@galt.com" model.email
            |> TextField.renderElement renderConfig
        , Button.fromLabel "Register user"
            |> Button.cmd RegisterUserClicked Button.primary
            |> Button.renderElement renderConfig
        ]
