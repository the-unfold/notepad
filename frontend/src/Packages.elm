module Packages exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import UI
import UI.Button as Button
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField


type Msg
    = RegisterUserClicked
    | EmailChanged String


type alias Model =
    { email : String }



-- view =
--     Element.el [] (Element.text "Packages page body newFunction")
-- view : RenderConfig -> b -> Element Msg
-- view renderConfig model =
--     Element.layout []
--         (Button.fromLabel "register user"
--             |> Button.cmd RegisterUserClicked Button.primary
--             |> Button.renderElement renderConfig
--         )


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
