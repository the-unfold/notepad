module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Fuck
import Html exposing (Html)
import Message as Msg
import Packages
import Page
import UI.Button as Button
import UI.Icon as Icon
import UI.Link as Link
import UI.NavigationContainer as Nav
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField


type alias Model =
    { email : String, navState : Nav.State, currentPage : Page.Page }


main : Program () Model Msg.Msg
main =
    Browser.document
        { init = init
        , subscriptions = always Sub.none
        , view = view renderCfg
        , update = update
        }


update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update msg model =
    -- case msg of
    --     Msg.NavMsg ->
    --     _ ->
    ( model, Cmd.none )


renderCfg : RenderConfig
renderCfg =
    RenderConfig.init { width = 1920, height = 1080 } RenderConfig.localeEnglish


init : () -> ( Model, Cmd Msg.Msg )
init _ =
    ( { email = "", navState = Nav.stateInit renderCfg, currentPage = Page.Packages }, Cmd.none )



-- testPage : Nav.Container
-- testPage =
--     { content = Nav.contentSingle <| Element.el [] (Element.text "Element body")
--     , title = "Example page"
--     , dialog = Nothing -- or Just <| Nav.dialog <| ...
--     , hasMenu = True
--     }


view : RenderConfig -> Model -> { body : List (Html Msg.Msg), title : String }
view renderConfig { navState, currentPage } =
    Nav.navigator Msg.NavMsg
        navState
        (getPageContainer >> Nav.containerMap Msg.PageMsg)
        |> Nav.withMenuPages
            [ Nav.menuPage (Icon.packages "Packages")
                (Link.link "/packages")
                (currentPage == Page.Packages)
            , Nav.menuPage (Icon.pause "Fuck")
                (Link.link "/fuck")
                (currentPage == Page.Fuck)
            ]
        |> Nav.withMenuActions
            [ Nav.menuAction
                (Icon.logout "Logout")
                Msg.SessionLogout
            ]
        |> Nav.withMenuLogo "My company's logo" (Element.text "Logo")
        |> Nav.toBrowserDocument renderConfig currentPage


getPageContainer : Page.Page -> Nav.Container Page.Msg
getPageContainer page =
    case page of
        Page.Packages ->
            { title = "Packages"
            , content = Nav.contentSingle <| Element.map Page.PackagesMsg (Packages.view renderCfg { email = "" })
            , dialog = Nothing
            , hasMenu = True
            }

        Page.Fuck ->
            { title = "Fuck"
            , content = Nav.contentSingle Fuck.view
            , dialog = Nothing
            , hasMenu = True
            }
