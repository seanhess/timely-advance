module Page.Home exposing (Model, init, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    ()


init : Model
init =
    ()


view : Model -> Html msg
view _ =
    div [] [ text "Onboard" ]
