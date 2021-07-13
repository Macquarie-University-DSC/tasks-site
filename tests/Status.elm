module Status exposing (..)

import Main
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import Time


suite : Test
suite =
    describe "Testing the http status views"
        [ describe "viewTasks"
            [ test "Loading response" <|
                \_ ->
                    let
                        testModel =
                            Main.Model Main.Loading Time.utc
                    in
                    Main.viewTasks testModel
                        |> Query.fromHtml
                        |> Query.has [ tag "h3", text "Loading..." ]
            , test "Failure response" <|
                \_ ->
                    let
                        testModel =
                            Main.Model Main.Failure Time.utc
                    in
                    Main.viewTasks testModel
                        |> Query.fromHtml
                        |> Query.has [ tag "h3", text "Error, could not connect to api" ]
            ]
        ]
