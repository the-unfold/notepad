module Main exposing (..)

import Browser
import Browser.Navigation as BNav
import Element
import Flags exposing (Flags)
import Html exposing (Html)
import Message as Msg
import Notes
import Packages
import Page
import UI.Icon as Icon
import UI.Link as Link
import UI.NavigationContainer as Nav
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import Url
import Url.Parser as Parser exposing (Parser, oneOf, s)


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
    { email : String
    , navState : Nav.State
    , currentPage : Page.Page
    , packagesModel : Packages.Model
    , notesModel : Notes.Model
    , flags : Flags
    }


main : Program Flags Model Msg.Msg
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

        Msg.NavMsg navMsg ->
            let
                ( newNavState, navCommand ) =
                    Nav.stateUpdate navMsg model.navState
            in
            ( { model | navState = newNavState }, Cmd.map Msg.NavMsg navCommand )

        Msg.PageMsg (Page.PackagesMsg packagesMsg) ->
            let
                ( newPackagesModel, packagesCommand ) =
                    Packages.update packagesMsg model.packagesModel
            in
            ( { model | packagesModel = newPackagesModel }, Cmd.map (Page.PackagesMsg >> Msg.PageMsg) packagesCommand )

        Msg.PageMsg (Page.NotesMsg notesMsg) ->
            let
                ( newNotesModel, notesCommand ) =
                    Notes.update notesMsg model.notesModel
            in
            ( { model | notesModel = newNotesModel }, Cmd.map (Page.NotesMsg >> Msg.PageMsg) notesCommand )

        _ ->
            ( model, Cmd.none )


renderCfg : RenderConfig
renderCfg =
    RenderConfig.init { width = 1920, height = 1080 } RenderConfig.localeEnglish


init : Flags -> Url.Url -> BNav.Key -> ( Model, Cmd Msg.Msg )
init flags _ _ =
    ( { email = ""
      , navState = Nav.stateInit renderCfg
      , currentPage = Page.Packages
      , packagesModel = Packages.init flags
      , notesModel = Notes.init ()
      , flags = flags
      }
    , Cmd.none
    )


view : RenderConfig -> Model -> { body : List (Html Msg.Msg), title : String }
view renderConfig model =
    Nav.navigator Msg.NavMsg
        model.navState
        (getPageContainer model >> Nav.containerMap Msg.PageMsg)
        |> Nav.withMenuPages
            [ Nav.menuPage (Icon.packages "Packages")
                (Link.link "/note/1")
                (model.currentPage == Page.Packages)
            , Nav.menuPage (Icon.pause "Notes")
                (Link.link "/notes")
                (model.currentPage == Page.Notes)
            ]
        |> Nav.withMenuActions
            [ Nav.menuAction
                (Icon.logout "Logout")
                Msg.SessionLogout
            ]
        |> Nav.withMenuLogo "My company's logo" (Element.text "Logo")
        |> Nav.toBrowserDocument renderConfig model.currentPage


getPageContainer : Model -> Page.Page -> Nav.Container Page.Msg
getPageContainer model page =
    case page of
        Page.Packages ->
            { title = "Packages"
            , content = Nav.contentSingle <| Element.map Page.PackagesMsg <| Packages.view renderCfg model.packagesModel
            , dialog = Nothing
            , hasMenu = True
            }

        Page.Notes ->
            { title = "Notes"
            , content = Nav.contentSingle <| Element.map Page.NotesMsg <| Notes.view renderCfg model.notesModel
            , dialog = Nothing
            , hasMenu = True
            }
