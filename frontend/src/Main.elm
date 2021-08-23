module Main exposing (..)

import Browser
import Browser.Navigation as BNav
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import Message as Msg
import Notes
import Packages
import Page
import UI.Button as Button
import UI.Icon as Icon
import UI.Link as Link
import UI.NavigationContainer as Nav
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type alias TimesGiven =
    String


type Route
    = Packages
    | Notes


fuckUrlParser : Parser.Parser (TimesGiven -> a) a
fuckUrlParser =
    Parser.custom "FUCKSGIVEN" Just


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Packages (s "packages")
        , Parser.map Notes (s "notes")
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
        Just Notes ->
            Page.Notes

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


view : RenderConfig -> Model -> { body : List (Html Msg.Msg), title : String }
view renderConfig { navState, currentPage } =
    Nav.navigator Msg.NavMsg
        navState
        (getPageContainer >> Nav.containerMap Msg.PageMsg)
        |> Nav.withMenuPages
            [ Nav.menuPage (Icon.packages "Packages")
                (Link.link "/note/1")
                (currentPage == Page.Packages)
            , Nav.menuPage (Icon.pause "Notes")
                (Link.link "/notes")
                (currentPage == Page.Notes)
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

        Page.Notes ->
            { title = "Notes"
            , content = Nav.contentSingle <| Element.map Page.NotesMsg <| Notes.view renderCfg { notes = [ Notes.Note { id = 1, content = "Fucking note" } ] }
            , dialog = Nothing
            , hasMenu = True
            }
