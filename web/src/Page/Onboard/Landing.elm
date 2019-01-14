module Page.Onboard.Landing exposing (Model, Msg, init, update, view)

import Element exposing (Element, column, el, link, padding, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Route exposing (Onboard(..), Route(..))
import Timely.Style as Style


type alias Model =
    ()


init : Model
init =
    ()


type alias Msg =
    ()


update : Model -> ( Model, Cmd Msg )
update model =
    ( model, Cmd.none )


view : Model -> Element msg
view _ =
    column [ padding 10, spacing 8 ]
        [ el [ Region.heading 1 ] (text "Timely Advance")
        , link Style.button { url = Route.url (Onboard Phone), label = text "Sign Up" }
        ]
