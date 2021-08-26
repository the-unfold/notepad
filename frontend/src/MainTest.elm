module MainTest exposing (..)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Main"
        [ describe "test runner still works"
            [ test "True is still True" <|
                \() ->
                    True
                        |> Expect.equal True
            ]
        ]
