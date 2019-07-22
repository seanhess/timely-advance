module Page.Settings.Main exposing (Model, Msg, init, update, view)

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
import Timely.Api as Api
import Timely.Components as Components
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.Account exposing (AccountId)


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
    = Logout
    | LogoutDone (Result Error String)


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update nav msg model =
    case msg of
        Logout ->
            updates model |> command (Api.sessionsLogout LogoutDone)

        LogoutDone _ ->
            updates model |> command (Route.goLanding nav)


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.header
            [ row [ spacing 15 ]
                [ Components.backLink (Route.Account model.accountId Route.AccountMain)
                , paragraph Style.heading [ text "Account Settings" ]
                ]
            ]
        , column Style.section
            [ link (Style.button Style.secondary)
                { url = Route.url (Route.Settings model.accountId Route.Subscription)
                , label = text "Modify Subscription"
                }
            , button [ Style.link, centerX ]
                { onPress = Just Logout
                , label = text "Logout"
                }
            ]
        ]
