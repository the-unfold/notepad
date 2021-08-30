module Notes exposing (..)

import Api.Note exposing (Note, noteDecoder)
import Api.NoteCreatePayload exposing (NoteCreatePayload, noteCreatePayloadEncoder)
import Element exposing (..)
import Element.Events exposing (onClick)
import Flags exposing (Flags)
import Http
import Json.Decode as D
import UI.Button as Button
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField
import WithUuid exposing (encodeWithUuid)


type Msg
    = NoteClicked Int
    | NoteContentChanged String
    | AddNoteClicked
    | GotCreateNoteResult
    | GotNotes (Result Http.Error (List Note))


type alias Model =
    { selectedId : Maybe Int, notes : List Note, noteContent : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { selectedId = Nothing
      , notes = []
      , noteContent = ""
      }
    , fetchNotes flags
    )


addNote : Flags -> NoteCreatePayload -> Cmd Msg
addNote flags noteCreatePayload =
    Http.post
        { url = flags.backendUrl ++ "/notes/create"
        , body =
            noteCreatePayload
                |> encodeWithUuid noteCreatePayloadEncoder "550e8400-e29b-41d4-a726-446655440047"
                |> Http.jsonBody
        , expect = Http.expectWhatever (always GotCreateNoteResult)
        }


fetchNotes : Flags -> Cmd Msg
fetchNotes flags =
    Http.get
        { url = flags.backendUrl ++ "/notes"
        , expect = Http.expectJson GotNotes (D.list noteDecoder)
        }


update : Flags -> Msg -> Model -> ( Model, Cmd Msg )
update flags msg model =
    case msg of
        NoteClicked noteId ->
            ( { model | selectedId = Just noteId }, Cmd.none )

        NoteContentChanged content ->
            ( { model | noteContent = content }, Cmd.none )

        AddNoteClicked ->
            ( model, addNote flags { content = model.noteContent } )

        GotCreateNoteResult ->
            ( { model | noteContent = "" }, Cmd.none )

        GotNotes result ->
            case result of
                Ok notes ->
                    ( { model | notes = notes }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : RenderConfig -> Model -> Element Msg
view renderConfig model =
    let
        viewNote : Note -> Element Msg
        viewNote { noteId, content } =
            row [ width fill, onClick <| NoteClicked noteId ]
                [ content
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
        , TextField.singlelineText NoteContentChanged "Content goes here" model.noteContent
            |> TextField.renderElement renderConfig
        , Button.fromLabel "Add Note"
            |> Button.cmd AddNoteClicked Button.primary
            |> Button.renderElement renderConfig
        ]
