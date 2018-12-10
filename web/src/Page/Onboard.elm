module Page.Onboard exposing (Model, Msg, init, subscriptions, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    ()


init : Model
init =
    ()


type alias Msg =
    ()


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html msg
view _ =
    div [] [ text "Onboard" ]
