module Api.NoteAddedPayload exposing
    ( NoteAddedPayload
    , noteAddedPayloadEncoder
    )

import Json.Encode


type alias NoteAddedPayload =
    { content : String }


noteAddedPayloadEncoder : NoteAddedPayload -> Json.Encode.Value
noteAddedPayloadEncoder a =
    Json.Encode.object [ ( "content", Json.Encode.string a.content ) ]
