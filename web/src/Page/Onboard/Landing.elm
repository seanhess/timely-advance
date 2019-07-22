module Page.Onboard.Landing exposing (Model, Msg, init, update, view)

import Element exposing (..)
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
    column Style.page
        [ column Style.info
            [ paragraph Style.heading [ text "Welcome to Timely Advance" ] ]
        , column Style.section
            [ paragraph [ centerX ] [ text "Get up to $500 worry free and put an end to overdrafts and tight spots while waiting for payday" ]
            , link (Style.button Style.primary) { url = Route.url (Onboard Signup), label = Style.label "Get Started" }
            , paragraph [ centerX ] [ text "Already have an account?" ]
            , link [ Style.link, centerX ] { url = Route.url (Onboard Login), label = text "Log in" }
            ]
        ]
