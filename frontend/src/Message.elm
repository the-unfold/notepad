module Message exposing (..)

import Browser
import Page
import UI.NavigationContainer as NavigationContainer
import Url


type Msg
    = PageMsg Page.Msg
    | NavMsg NavigationContainer.Msg
    | SessionLogout
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
