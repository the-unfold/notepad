module Notes exposing (..)

import Element exposing (..)
import Element.Events exposing (onClick)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


type Msg
    = NoteClicked Int


type Note
    = Note { id : Int, content : String }


type alias Model =
    { selectedId : Maybe Int, notes : List Note }


init : () -> Model
init _ =
    { selectedId = Nothing
    , notes = [ Note { id = 1, content = "Fucking note" } ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update (NoteClicked noteId) model =
    ( { model | selectedId = Just noteId }, Cmd.none )


view : RenderConfig -> Model -> Element Msg
view renderConfig model =
    let
        viewNote : Note -> Element Msg
        viewNote (Note note) =
            row [ width fill, onClick <| NoteClicked note.id ]
                [ note.content
                    |> Text.body1
                    |> Text.withColor (Palette.color Palette.toneGray Palette.brightnessDarkest)
                    |> Text.renderElement renderConfig
                ]
    in
    column [ width fill, alignTop, padding 10, spacing 20 ]
        [ "Your fucking notes"
            |> Text.heading1
            |> Text.withColor (Palette.color Palette.toneGray Palette.brightnessDarkest)
            |> Text.renderElement renderConfig
        , column [ width fill, alignTop, padding 10, spacing 20 ]
            (List.map viewNote model.notes)
        ]
