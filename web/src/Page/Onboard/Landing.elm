module Page.Onboard.Landing exposing (Model, Msg, init, update, view)

import Element exposing (Element, column, el, link, padding, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api
import Timely.Style as Style


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column [ padding 10, spacing 8 ]
        [ el [ Region.heading 1 ] (text "Timely Advance")
        , link Style.button { url = Route.url (Onboard Signup), label = text "Get Started" }
        , link [] { url = Route.url (Onboard Login), label = text "Log in" }
        ]
