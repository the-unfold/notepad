module Page exposing (..)

import Notes
import Users


type Page
    = Users -- Users.Model
    | Notes -- Fuck.Model


type Msg
    = UsersMsg Users.Msg
    | NotesMsg Notes.Msg
