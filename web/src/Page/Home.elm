module Page.Home exposing (Model, Msg, init, subscriptions, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    ()


type alias Msg =
    ()


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


init : Model
init =
    ()


view : Model -> Html msg
view _ =
    div [] [ text "Home" ]
