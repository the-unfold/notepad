module Main exposing (..)

import Browser
import Browser.Navigation as BNav
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
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type alias TimesGiven =
    String


type Route
    = Packages
    | Fuck TimesGiven


fuckUrlParser : Parser.Parser (TimesGiven -> a) a
fuckUrlParser =
    Parser.custom "FUCKSGIVEN" Just


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Packages (s "packages")
        , Parser.map Fuck (s "fuck" </> fuckUrlParser)
        ]


type alias Model =
    { email : String, navState : Nav.State, currentPage : Page.Page }


main : Program () Model Msg.Msg
main =
    Browser.application
        { init = init
        , subscriptions = always Sub.none
        , view = view renderCfg
        , update = update
        , onUrlChange = Msg.UrlChanged
        , onUrlRequest = Msg.LinkClicked
        }


fromUrl : Url.Url -> Maybe Route
fromUrl =
    Parser.parse parser


routeToPage : Maybe Route -> Page.Page
routeToPage route =
    case route of
        Just ((Fuck _) as x) ->
            Page.Fuck

        Just Packages ->
            Page.Packages

        _ ->
            Page.Packages


updatePageFromUrl : Url.Url -> Model -> Model
updatePageFromUrl url model =
    { model | currentPage = (fromUrl >> routeToPage) url }


update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.UrlChanged url ->
            ( updatePageFromUrl url model, Cmd.none )

        Msg.LinkClicked x ->
            case x of
                Browser.Internal url ->
                    ( updatePageFromUrl url model, Cmd.none )

                Browser.External _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


renderCfg : RenderConfig
renderCfg =
    RenderConfig.init { width = 1920, height = 1080 } RenderConfig.localeEnglish


init : () -> Url.Url -> BNav.Key -> ( Model, Cmd Msg.Msg )
init _ _ _ =
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
                (Link.link "/fuck/me")
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
            , content = Nav.contentSingle <| Element.map Page.PackagesMsg <| Packages.view renderCfg { email = "" }
            , dialog = Nothing
            , hasMenu = True
            }

        Page.Fuck ->
            { title = "Fuck"
            , content = Nav.contentSingle Fuck.view
            , dialog = Nothing
            , hasMenu = True
            }
