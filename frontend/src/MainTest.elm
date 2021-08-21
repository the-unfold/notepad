module MainTest exposing (..)

import Main
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)


suite : Test
suite =
    describe "Main"
        [ describe "view"
            [ test "Shows app title" <|
                \() ->
                    Main.view { email = "john@galt.com" }
                        |> Query.fromHtml
                        |> Query.has [ text "Notepad" ]
            ]
        ]
