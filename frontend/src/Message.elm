module Message exposing (..)

import Browser
import Page
import UI.NavigationContainer as Nav
import Url


type CustomPageMsg
    = CustomPageMsg


type Msg
    = PageMsg Page.Msg
    | NavMsg Nav.Msg
    | SessionLogout
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
