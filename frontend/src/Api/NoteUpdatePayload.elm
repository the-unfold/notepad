module Api.NoteUpdatePayload exposing
    ( NoteUpdatePayload
    , noteUpdatePayloadDecoder
    , noteUpdatePayloadEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias NoteUpdatePayload =
    { noteId : Int, content : String }


noteUpdatePayloadEncoder : NoteUpdatePayload -> Json.Encode.Value
noteUpdatePayloadEncoder a =
    Json.Encode.object
        [ ( "noteId", Json.Encode.int a.noteId )
        , ( "content", Json.Encode.string a.content )
        ]


noteUpdatePayloadDecoder : Json.Decode.Decoder NoteUpdatePayload
noteUpdatePayloadDecoder =
    Json.Decode.succeed NoteUpdatePayload
        |> Json.Decode.Pipeline.required "noteId" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
