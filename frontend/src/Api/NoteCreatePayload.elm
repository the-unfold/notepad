module Api.NoteCreatePayload exposing
    ( NoteCreatePayload
    , noteCreatePayloadDecoder
    , noteCreatePayloadEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias NoteCreatePayload =
    { content : String }


noteCreatePayloadEncoder : NoteCreatePayload -> Json.Encode.Value
noteCreatePayloadEncoder a =
    Json.Encode.object [ ( "content", Json.Encode.string a.content ) ]


noteCreatePayloadDecoder : Json.Decode.Decoder NoteCreatePayload
noteCreatePayloadDecoder =
    Json.Decode.succeed NoteCreatePayload
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
