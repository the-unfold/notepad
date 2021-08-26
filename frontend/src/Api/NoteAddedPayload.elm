module Api.NoteAddedPayload exposing
    ( NoteAddedPayload
    , noteAddedPayloadDecoder
    , noteAddedPayloadEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias NoteAddedPayload =
    { content : String }


noteAddedPayloadEncoder : NoteAddedPayload -> Json.Encode.Value
noteAddedPayloadEncoder a =
    Json.Encode.object [ ( "content", Json.Encode.string a.content ) ]


noteAddedPayloadDecoder : Json.Decode.Decoder NoteAddedPayload
noteAddedPayloadDecoder =
    Json.Decode.succeed NoteAddedPayload
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
