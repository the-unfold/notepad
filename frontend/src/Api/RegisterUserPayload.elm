module Api.RegisterUserPayload exposing
    ( RegisterUserPayload
    , registerUserPayloadEncoder
    )

import Json.Encode


type alias RegisterUserPayload =
    { email : String }


registerUserPayloadEncoder : RegisterUserPayload -> Json.Encode.Value
registerUserPayloadEncoder a =
    Json.Encode.object [ ( "email", Json.Encode.string a.email ) ]
