module Page.Account.Spending exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Attributes as Html
import Http exposing (Error)
import Page.Account.Budget exposing (formatSchedule)
import Platform.Updates exposing (Updates, command, modify, updates)
import Route
import Timely.Api as Api
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.Account exposing (AccountId)
import Timely.Types.AccountHealth exposing (AccountHealth)
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetType(..))
import Timely.Types.Date as Date exposing (Date, formatDate)
import Timely.Types.Money as Money exposing (Money, formatMoney)
import Timely.Types.Transactions exposing (Schedule(..))


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , amount : String

    -- , bills : Resource (List (BudgetId Budget))
    }


type Msg
    = Close
    | Save
    | Edit String
    | OnSaved (Result Http.Error String)



-- | OnBills (Result Http.Error (List (BudgetId Budget)))
-- | AddBill
-- | OnCreated (Result Http.Error (Id Budget))


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { key = key
      , accountId = accountId
      , amount = ""
      }
    , Cmd.batch []
    )


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        goBack =
            Route.goAccount model.key model.accountId
    in
    case msg of
        Close ->
            updates model |> command goBack

        Save ->
            -- no, I want to throw an error if it isn't good
            case Maybe.map Money.fromDollars (String.toFloat model.amount) of
                Nothing ->
                    -- TODO display an error if it isn't valid
                    -- TODO don't allow them to accept if not valid?
                    updates model

                Just a ->
                    updates model |> command (Api.putSpending OnSaved model.accountId a)

        Edit s ->
            updates { model | amount = s }

        OnSaved _ ->
            updates model |> command goBack


view =
    viewPopup


viewPage : Model -> Element Msg
viewPage model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Close
                , paragraph [] [ text "Your daily spending" ]
                ]
            ]
        , column Style.section
            -- [ resource (viewSpending model.accountId) model.bills
            [ viewSpending model.accountId model.amount
            ]
        ]


viewPopup : Model -> Element Msg
viewPopup model =
    column Style.section
        [ row [ spacing 15, width fill ]
            [ text "Edit Your Spending"
            , el [ alignRight ] (Components.close Close)
            ]
        , viewSpending model.accountId model.amount
        ]


viewSpending : Id AccountId -> String -> Element Msg
viewSpending accountId amount =
    column [ spacing 10, width fill ]
        [ Input.text [ htmlAttribute (Html.type_ "number") ]
            { text = amount
            , placeholder = Just <| Input.placeholder [] (text "$30")
            , onChange = Edit
            , label = Input.labelAbove [ Font.size 14 ] (text "Amount (USD)")
            }
        , button (Style.button Style.primary) { onPress = Just Save, label = text "Save" }
        ]



-- , row [ spacing 10 ]
--     [ button (Style.button Style.secondary) { onPress = Just AddBill, label = text "Add Bill" }
--     , button (Style.button Style.secondary) { onPress = Just AddPaycheck, label = text "Add Paycheck" }
--     ]
-- viewEvent : Id AccountId -> Event -> Element Msg
-- viewEvent accountId event =
--     button [ width fill ]
--         { onPress = Just (Select event)
--         , label =
--             wrappedRow [ spacing 10, width fill ]
--                 [ el [ width (px 125) ] (text <| formatDate event.transaction.date)
--                 , row [ width (px 80) ] [ el [ alignRight ] (text <| formatMoney event.transaction.amount) ]
--                 , el [] (text <| event.transaction.name)
--                 , el [ alignRight, Font.color (healthy event.balance) ] (text <| formatMoney event.balance)
--                 ]
--         }
-- viewEdit : Id AccountId -> Element msg
-- viewEdit accountId =
--     link [ Style.link ]
--         { url = Route.url (Route.Account accountId Route.Budgets)
--         , label = text "Edit Income and Bills"
--         }
-- viewIncome : Budget -> Element msg
-- viewIncome income =
--     column [ spacing 6, width fill ]
--         [ row [ spacing 8, width fill ]
--             [ text income.name
--             , el [ alignRight ] (text (formatMoney income.amount))
--             ]
--         , el [ Font.size 16 ] (text <| formatSchedule income.schedule)
--         ]
