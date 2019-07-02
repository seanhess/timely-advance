module Page.Settings.Subscription exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Http exposing (Error)
import Platform.Updates exposing (Updates, command, set, updates)
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (AccountId)
import Timely.Components as Components
import Timely.Style as Style
import Timely.Types exposing (Id(..))


type alias Model =
    { accountId : Id AccountId
    }


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      }
    , Cmd.none
    )


type Msg
    = None


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update nav msg model =
    case msg of
        None ->
            updates model


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.header
            [ row [ spacing 15 ]
                [ Components.backLink (Route.Settings model.accountId Route.SettingsMain)
                , el Style.heading (text "Manage Subscription")
                ]
            ]
        , column Style.section
            []
        ]
