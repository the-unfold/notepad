module Api.Either exposing
    ( Either(..)
    , decoder
    , encoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Either a b
    = Left a
    | Right b


encoder : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> Either a b -> Json.Encode.Value
encoder a b c =
    case c of
        Left d ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Left" )
                , ( "contents", a d )
                ]

        Right d ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Right" )
                , ( "contents", b d )
                ]


decoder : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Either a b)
decoder a b =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\c ->
                case c of
                    "Left" ->
                        Json.Decode.succeed Left
                            |> Json.Decode.Pipeline.required "contents" a

                    "Right" ->
                        Json.Decode.succeed Right
                            |> Json.Decode.Pipeline.required "contents" b

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )
