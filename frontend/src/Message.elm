module Message exposing (..)

import Page
import UI.NavigationContainer as Nav


type CustomPageMsg
    = CustomPageMsg


type Msg
    = PageMsg Page.Msg
    | NavMsg Nav.Msg
    | SessionLogout
