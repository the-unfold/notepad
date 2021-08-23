module Notes exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import UI.Button as Button
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField


type Msg
    = NoteClicked Int


type Note
    = Note { id : Int, content : String }


type alias Model =
    { notes : List Note }


view : RenderConfig -> Model -> Element Msg
view renderConfig model =
    let
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
