module Main exposing (..)

import Browser
import Browser.Navigation as BNav
import Element
import Flags exposing (Flags)
import Html exposing (Html)
import Message as Msg
import Notes
import Page
import UI.Icon as Icon
import UI.Link as Link
import UI.NavigationContainer as Nav
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import Url
import Url.Parser as Parser exposing (Parser, oneOf, s)
import Users


type alias TimesGiven =
    String


type Route
    = Users
    | Notes


fuckUrlParser : Parser.Parser (TimesGiven -> a) a
fuckUrlParser =
    Parser.custom "FUCKSGIVEN" Just


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Users (s "users")
        , Parser.map Notes (s "notes")
        ]


type alias Model =
    { email : String
    , navState : Nav.State
    , currentPage : Page.Page
    , usersModel : Users.Model
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

        Just Users ->
            Page.Users

        _ ->
            Page.Users


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

        Msg.PageMsg (Page.UsersMsg usersMsg) ->
            let
                ( newUsersModel, usersCmd ) =
                    Users.update usersMsg model.usersModel
            in
            ( { model | usersModel = newUsersModel }, Cmd.map (Page.UsersMsg >> Msg.PageMsg) usersCmd )

        Msg.PageMsg (Page.NotesMsg notesMsg) ->
            let
                ( newNotesModel, notesCommand ) =
                    Notes.update model.flags notesMsg model.notesModel
            in
            ( { model | notesModel = newNotesModel }, Cmd.map (Page.NotesMsg >> Msg.PageMsg) notesCommand )

        _ ->
            ( model, Cmd.none )


renderCfg : RenderConfig
renderCfg =
    RenderConfig.init { width = 1920, height = 1080 } RenderConfig.localeEnglish


init : Flags -> Url.Url -> BNav.Key -> ( Model, Cmd Msg.Msg )
init flags _ _ =
    let
        ( notesModel, notesCmd ) =
            Notes.init flags
    in
    ( { email = ""
      , navState = Nav.stateInit renderCfg
      , currentPage = Page.Users
      , usersModel = Users.init flags
      , notesModel = notesModel
      , flags = flags
      }
    , Cmd.map (Msg.PageMsg << Page.NotesMsg) notesCmd
    )


view : RenderConfig -> Model -> { body : List (Html Msg.Msg), title : String }
view renderConfig model =
    Nav.navigator Msg.NavMsg
        model.navState
        (getPageContainer model >> Nav.containerMap Msg.PageMsg)
        |> Nav.withMenuPages
            [ Nav.menuPage (Icon.packages "Users")
                (Link.link "/note/1")
                (model.currentPage == Page.Users)
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
        Page.Users ->
            { title = "Users"
            , content = Nav.contentSingle <| Element.map Page.UsersMsg <| Users.view renderCfg model.usersModel
            , dialog = Nothing
            , hasMenu = True
            }

        Page.Notes ->
            { title = "Notes"
            , content = Nav.contentSingle <| Element.map Page.NotesMsg <| Notes.view renderCfg model.notesModel
            , dialog = Nothing
            , hasMenu = True
            }
