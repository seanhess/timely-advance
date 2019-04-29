module Page.Admin.Customer exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (AccountId)
import Timely.Style as Style
import Timely.Types exposing (Id)


type alias Model =
    { accountId : Id AccountId }


init : Id AccountId -> ( Model, Cmd Msg )
init i =
    ( { accountId = i }, Cmd.none )


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column []
        [ text "Customer" ]
