module Api.WithUuid exposing
    ( WithUuid
    , decoder
    , encoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias WithUuid a =
    { payload : a, uuid : String }


encoder : (a -> Json.Encode.Value) -> WithUuid a -> Json.Encode.Value
encoder a b =
    Json.Encode.object
        [ ( "payload", a b.payload )
        , ( "uuid", Json.Encode.string b.uuid )
        ]


decoder : Json.Decode.Decoder a -> Json.Decode.Decoder (WithUuid a)
decoder a =
    Json.Decode.succeed WithUuid
        |> Json.Decode.Pipeline.required "payload" a
        |> Json.Decode.Pipeline.required "uuid" Json.Decode.string
