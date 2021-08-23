module MainTest exposing (..)

import Expect
import Main
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)


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
