module WithUuid exposing (encodeWithUuid)

import Json.Encode as E


encodeWithUuid : (a -> E.Value) -> String -> a -> E.Value
encodeWithUuid encoder uuid value =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "payload", encoder value )
        ]
