module Page.Onboard exposing (view, Model, init)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model = ()

init : Model
init = ()

view : Model -> Html msg
view _ = div [ ] [ text "Onboard" ]

