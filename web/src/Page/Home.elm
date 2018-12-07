module Page.Home exposing (view, Model)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = ()

view : Model -> Html msg
view _ = div [ ] [ text "Onboard" ]
