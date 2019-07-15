module Page.Admin.Customer exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Http
import Platform.Updates exposing (Updates, command, set, updates)
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (Customer)
import Timely.Icons as Icons exposing (Size(..))
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id, Valid(..))
import Timely.Types.Account exposing (AccountId)
import Timely.Types.Date exposing (formatDate)


type alias Model =
    { accountId : Id AccountId
    , customer : Resource Customer
    , wantsDelete : Bool
    }


init : Id AccountId -> ( Model, Cmd Msg )
init i =
    ( { accountId = i
      , customer = Loading
      , wantsDelete = False
      }
    , Api.getCustomer OnCustomer i
    )


type Msg
    = OnCustomer (Result Http.Error Customer)
    | WantsDelete Bool
    | Delete
    | OnDone (Result Http.Error String)


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    case msg of
        OnCustomer rc ->
            updates { model | customer = Resource.fromResult rc }

        WantsDelete w ->
            updates { model | wantsDelete = w }

        Delete ->
            updates model
                |> command (Api.deleteAccount OnDone model.accountId)

        OnDone _ ->
            updates model
                |> command (Route.pushUrl key (Route.Admin Route.Sudo))


view : Model -> Element Msg
view model =
    column Style.section
        [ resource (viewCustomer model) model.customer ]


viewCustomer : Model -> Customer -> Element Msg
viewCustomer model cust =
    let
        fromValid =
            \(Valid v) -> v
    in
    column [ spacing 10, width fill ]
        [ link [] { url = Route.url (Route.Admin Route.Sudo), label = Icons.icon Icons.back Icons.Big }
        , el [ Font.bold ]
            (text <|
                String.join " "
                    [ cust.firstName
                    , Maybe.withDefault "" cust.middleName
                    , cust.lastName
                    ]
            )
        , el [] (text cust.email)

        -- , el [] (text <| fromValid cust.ssn)
        -- , el [] (text <| formatDate cust.dateOfBirth)
        , link (Style.button Style.primary)
            { url = Route.url (Route.Account cust.accountId Route.AccountMain)
            , label = text "Account"
            }
        , viewDelete model.wantsDelete
        ]


viewDelete : Bool -> Element Msg
viewDelete wants =
    if wants then
        column [ spacing 10, width fill ]
            [ el [] (text "Warning: this is permanant!")
            , row [ spacing 10, width fill ]
                [ button (Style.button Style.secondary)
                    { onPress = Just (WantsDelete False)
                    , label = text "Nevermind"
                    }
                , button (Style.button Style.destroy)
                    { onPress = Just Delete
                    , label = text "Delete"
                    }
                ]
            ]

    else
        button [ Style.link, Font.color Style.lightRed ]
            { onPress = Just (WantsDelete True)
            , label = text "Delete"
            }
