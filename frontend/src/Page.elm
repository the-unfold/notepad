module Page exposing (..)

import Fuck
import Packages


type Page
    = Packages -- Packages.Model
    | Fuck -- Fuck.Model


type Msg
    = PackagesMsg Packages.Msg
    | FuckMsg Fuck.Msg
