module Api.Note exposing
    ( Note
    , noteDecoder
    , noteEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Note =
    { noteId : Int, content : String }


noteEncoder : Note -> Json.Encode.Value
noteEncoder a =
    Json.Encode.object
        [ ( "noteId", Json.Encode.int a.noteId )
        , ( "content", Json.Encode.string a.content )
        ]


noteDecoder : Json.Decode.Decoder Note
noteDecoder =
    Json.Decode.succeed Note
        |> Json.Decode.Pipeline.required "noteId" Json.Decode.int
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
