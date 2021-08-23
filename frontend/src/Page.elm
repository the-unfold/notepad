module Page exposing (..)

import Notes
import Packages


type Page
    = Packages -- Packages.Model
    | Notes -- Fuck.Model


type Msg
    = PackagesMsg Packages.Msg
    | NotesMsg Notes.Msg
