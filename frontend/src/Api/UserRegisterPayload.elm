module Api.UserRegisterPayload exposing
    ( UserRegisterPayload
    , userRegisterPayloadDecoder
    , userRegisterPayloadEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias UserRegisterPayload =
    { email : String }


userRegisterPayloadEncoder : UserRegisterPayload -> Json.Encode.Value
userRegisterPayloadEncoder a =
    Json.Encode.object [ ( "email", Json.Encode.string a.email ) ]


userRegisterPayloadDecoder : Json.Decode.Decoder UserRegisterPayload
userRegisterPayloadDecoder =
    Json.Decode.succeed UserRegisterPayload
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
