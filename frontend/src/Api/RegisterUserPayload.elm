module Api.RegisterUserPayload exposing
    ( RegisterUserPayload
    , registerUserPayloadDecoder
    , registerUserPayloadEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias RegisterUserPayload =
    { email : String }


registerUserPayloadEncoder : RegisterUserPayload -> Json.Encode.Value
registerUserPayloadEncoder a =
    Json.Encode.object [ ( "email", Json.Encode.string a.email ) ]


registerUserPayloadDecoder : Json.Decode.Decoder RegisterUserPayload
registerUserPayloadDecoder =
    Json.Decode.succeed RegisterUserPayload
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
