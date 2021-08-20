module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)


type alias Model =
    { unit : () }


type Msg
    = GotUnit ()


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotUnit x) model =
    ( { model | unit = x }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { unit = () }, Cmd.none )


view : Model -> Html Msg
view _ =
    Element.layout
        [ Background.color (rgb255 255 255 255), Font.color (rgb255 0 0 0) ]
        (column [ width fill, alignTop, padding 10, spacing 20, onClick <| GotUnit () ]
            [ text "Notepad"
            ]
        )
