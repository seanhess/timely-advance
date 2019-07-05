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
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.Subscription exposing (Subscription)


type alias Model =
    { accountId : Id AccountId
    , subscription : Resource Subscription
    , subscriptions : Resource (List Subscription)
    }


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      , subscription = Loading
      , subscriptions = Loading
      }
    , Cmd.batch
        [ Api.getAvailableSubscriptions OnSubscriptions
        , Api.getSubscription OnSubscription accountId
        ]
    )


type Msg
    = OnSubscription (Result Error Subscription)
    | OnSubscriptions (Result Error (List Subscription))


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update nav msg model =
    case msg of
        OnSubscription r ->
            updates { model | subscription = Resource.fromResult r }

        OnSubscriptions r ->
            updates { model | subscriptions = Resource.fromResult r }


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
