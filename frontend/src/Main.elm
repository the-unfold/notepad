module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import UI.Button as Button
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField


type alias Model =
    { email : String }


type Msg
    = RegisterUserClicked
    | EmailChanged String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterUserClicked ->
            ( model, Cmd.none )

        EmailChanged email ->
            ( { model | email = email }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { email = "" }, Cmd.none )


renderCfg : RenderConfig
renderCfg =
    RenderConfig.init { width = 1920, height = 1080 } RenderConfig.localeEnglish


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 255 255 255), Font.color (rgb255 0 0 0) ]
        (column [ width fill, alignTop, padding 10, spacing 20, onClick <| RegisterUserClicked ]
            [ "Notepad"
                |> Text.heading1
                |> Text.withColor (Palette.color Palette.toneGray Palette.brightnessDarkest)
                |> Text.renderElement renderCfg
            , TextField.singlelineText EmailChanged "john@galt.com" model.email
                |> TextField.renderElement renderCfg
            , Button.fromLabel "Register user"
                |> Button.cmd RegisterUserClicked Button.primary
                |> Button.renderElement renderCfg
            ]
        )
