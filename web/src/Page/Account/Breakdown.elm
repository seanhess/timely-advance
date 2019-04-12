module Page.Account.Breakdown exposing (Model, Msg, init, update, view, viewBreakdown)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Http exposing (Error)
import Page.Account.Budgets exposing (formatSchedule)
import Route
import Time exposing (Zone)
import Timely.Api as Api exposing (AccountId, Id)
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types.AccountHealth exposing (AccountHealth, Bill, Budget)
import Timely.Types.Date as Date exposing (Date, TimeZone, formatDate)
import Timely.Types.Money as Money exposing (formatMoney)


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , health : Resource AccountHealth
    , zone : Zone
    }


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key id =
    ( { key = key
      , accountId = id
      , health = Loading
      , zone = Time.utc
      }
    , Cmd.batch
        [ Date.timezone OnTimeZone
        , Api.getAccountHealth OnHealth id
        ]
    )


type Msg
    = OnBack
    | OnTimeZone Zone
    | OnHealth (Result Http.Error AccountHealth)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnBack ->
            ( model, Route.pushUrl model.key <| Route.Account model.accountId Route.AccountMain )

        OnHealth re ->
            ( { model | health = Resource.fromResult re }, Cmd.none )

        OnTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )


view : Model -> Element Msg
view model =
    resource (viewBreakdown OnBack model.zone model.accountId) model.health


viewBreakdown : msg -> TimeZone -> Id AccountId -> AccountHealth -> Element msg
viewBreakdown onBack zone accountId health =
    let
        healthy val =
            if Money.toCents val >= 0 then
                Style.green

            else
                Style.lightRed
    in
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back onBack
                , paragraph [] [ text "Here's how we calculated your safe-to-spend" ]
                ]
            ]
        , column Style.section
            [ el Style.banner (text "Income")
            , viewIncome health.income
            , row Style.banner
                [ el [] (text "Balance")
                , el [ alignRight, Font.color (healthy health.balance) ] (text <| formatMoney health.balance)
                ]
            , row Style.banner
                [ el [] (text "Needed for Bills")
                , el [ alignRight, Font.color Style.lightRed ] (text <| "-" ++ formatMoney health.budgeted)
                ]
            , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] <| List.map (viewBill zone) health.bills
            , row Style.banner
                [ el [] (text "Safe to Spend")
                , el [ alignRight, Font.color (healthy health.spending) ] (text <| formatMoney health.spending)
                ]
            , viewEdit accountId
            ]
        ]


viewEdit : Id AccountId -> Element msg
viewEdit accountId =
    link [ Style.link ]
        { url = Route.url (Route.Account accountId Route.Budgets)
        , label = text "Edit Income and Bills"
        }


viewIncome : Budget -> Element msg
viewIncome income =
    column [ spacing 6, width fill ]
        [ row [ spacing 8, width fill ]
            [ text income.name
            , el [ alignRight ] (text (formatMoney income.amount))
            ]
        , el [ Font.size 16 ] (text <| formatSchedule income.schedule)
        ]


viewBill : TimeZone -> Bill -> Element msg
viewBill zone bill =
    column [ spacing 6, width fill, padding 10 ]
        [ row [ spacing 8, width fill ]
            [ text bill.budget.name
            , el [ alignRight ] (text <| formatMoney bill.saved)
            ]
        , wrappedRow [ Font.size 16 ]
            [ text <| formatMoney bill.budget.amount
            , text " - "
            , text <| formatDate zone bill.next
            , text " - "
            , text <| formatSchedule bill.budget.schedule
            ]
        ]
