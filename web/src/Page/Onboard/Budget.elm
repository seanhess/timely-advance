module Page.Onboard.Budget exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (AccountId, Id)
import Timely.Resource exposing (Resource(..))
import Timely.Style as Style
import Timely.Types.Transactions exposing (History)


type alias Model =
    { history : Resource History

    -- , income :
    }


init : Id AccountId -> ( Model, Cmd Msg )
init id =
    ( { history = Loading }
    , Api.getTransactionHistory OnHistory id
    )


type Msg
    = OnHistory (Result Http.Error History)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ el [] (text "Budget") ]
        ]
