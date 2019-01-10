module Page.Onboard exposing (Model, Msg, init, subscriptions, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Route exposing (Route(..))
import Timely.Style as Style


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


view : Model -> Element msg
view _ =
    column [ padding 10, spacing 8 ]
        [ el [ Region.heading 1 ] (text "The App")
        , link Style.button { url = Route.url Signup, label = text "Sign Up" }
        ]
