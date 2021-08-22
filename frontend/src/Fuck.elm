module Fuck exposing (..)

import Element


type Msg
    = Msg


type alias Model =
    { unit : () }


view : Element.Element msg
view =
    Element.el [] (Element.text "Fuck Element body")
