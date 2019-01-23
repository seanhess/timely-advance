module Page.Init exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (Element, column, el, link, padding, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (Session)
import Timely.Components as Components
import Timely.Style as Style


type alias Model =
    { key : Nav.Key
    }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key }, Api.sessionsGet LoadedSession )


type Msg
    = LoadedSession (Result Http.Error Session)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        onboard =
            Route.url <| Route.Onboard Route.Landing

        account i =
            Route.url <| Route.Account i
    in
    case msg of
        LoadedSession (Err e) ->
            ( model, Nav.pushUrl model.key onboard )

        LoadedSession (Ok s) ->
            case s.accountId of
                Nothing ->
                    ( model, Nav.pushUrl model.key onboard )

                Just i ->
                    ( model, Nav.pushUrl model.key (account i) )


view : Model -> Element Msg
view _ =
    Components.spinnerRipple
