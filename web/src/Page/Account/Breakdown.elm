module Page.Account.Breakdown exposing (viewBreakdown)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Http exposing (Error)
import Page.Account.Budget exposing (formatSchedule)
import Platform.Updates exposing (Updates, command, modify, updates)
import Route
import Timely.Api as Api exposing (AccountId)
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.AccountHealth exposing (AccountHealth)
import Timely.Types.Advance exposing (Advance)
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetType(..), scheduledDate)
import Timely.Types.Date as Date exposing (Date, formatDate)
import Timely.Types.Money as Money exposing (Money, formatMoney)
import Timely.Types.Transactions exposing (Schedule(..))



-- type alias Model =
--     { key : Nav.Key
--     , accountId : Id AccountId
--     , health : Resource AccountHealth
--     , paychecks : Resource (List (BudgetId Budget))
--     , bills : Resource (List (BudgetId Budget))
--     }
-- type Msg
--     = Close
--     | OnHealth (Result Http.Error AccountHealth)
--     | OnPaychecks (Result Http.Error (List (BudgetId Budget)))
--     | OnBills (Result Http.Error (List (BudgetId Budget)))
--       -- | Select Event
--     | AddBill
--     | AddPaycheck
--     | OnCreated BudgetType (Result Http.Error (Id Budget))
-- init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
-- init key accountId =
--     ( { key = key
--       , accountId = accountId
--       , health = Loading
--       , paychecks = Loading
--       , bills = Loading
--       }
--     , Cmd.batch
--         [ Api.getAccountHealth OnHealth accountId
--         , Api.getIncomes OnPaychecks accountId
--         , Api.getExpenses OnBills accountId
--         ]
--     )
-- update : Msg -> Model -> Updates Model Msg ()
-- update msg model =
--     let
--         isBudget : Budget -> BudgetId Budget -> Bool
--         isBudget b bi =
--             (b.name == bi.name)
--                 && (b.schedule == bi.schedule)
--                 && (b.amount == bi.amount)
--         findBudget : Budget -> List (BudgetId Budget) -> Maybe (BudgetId Budget)
--         findBudget bud bs =
--             bs
--                 |> List.filter (isBudget bud)
--                 |> List.head
--         goBudget : BudgetType -> BudgetId Budget -> Cmd Msg
--         goBudget typ b =
--             goBudgetId typ b.budgetId
--         goBudgetId : BudgetType -> Id Budget -> Cmd Msg
--         goBudgetId typ bid =
--             Route.pushUrl model.key (Route.Account model.accountId (Route.Budget typ bid))
--         -- navBudget : BudgetType -> Resource (List (BudgetId Budget)) -> Event -> Cmd Msg
--         -- navBudget bt rs event =
--         --     rs
--         --         |> Resource.toMaybe
--         --         |> Maybe.andThen (findBudget event.budget)
--         --         |> Maybe.map (goBudget bt)
--         --         |> Maybe.withDefault Cmd.none
--         defaultBudget : String -> Budget
--         defaultBudget name =
--             { name = name
--             , schedule = Monthly { date = 1 }
--             , amount = Money.fromCents 0
--             }
--     in
--     case msg of
--         Close ->
--             updates model
--                 |> command (Route.goAccount model.key model.accountId)
--         OnHealth re ->
--             updates { model | health = Resource.fromResult re }
--                 |> command (Route.checkUnauthorized model.key re)
--         OnPaychecks pays ->
--             updates { model | paychecks = Resource.fromResult pays }
--         OnBills bs ->
--             updates { model | bills = Resource.fromResult bs }
--         -- Select e ->
--         --     updates model
--         --         |> command (navBudget Income model.paychecks e)
--         --         |> command (navBudget Expense model.bills e)
--         AddBill ->
--             updates model
--                 |> command (Api.createExpense (OnCreated Expense) model.accountId (defaultBudget "New Bill"))
--         AddPaycheck ->
--             updates model
--                 |> command (Api.createIncome (OnCreated Income) model.accountId (defaultBudget "New Paycheck"))
--         OnCreated budgetType (Ok budgetId) ->
--             updates model
--                 |> command (goBudgetId budgetType budgetId)
--         OnCreated _ _ ->
--             updates model
-- view : Model -> Element Msg
-- view model =
--     column Style.page
--         [ column Style.info
--             [ row [ spacing 15 ]
--                 [ Components.back Close
--                 , paragraph [] [ text "Here's how we calculated your lowest balance" ]
--                 ]
--             ]
--         , column Style.section
--             [ resource (viewBreakdown model.accountId) model.health
--             ]
--         ]


viewBreakdown : Id AccountId -> AccountHealth -> Budget -> Element msg
viewBreakdown accountId health paycheck =
    column [ spacing 20, width fill ]
        [ row Style.banner
            [ el [] (text "Current Balance")
            , el [ alignRight, Font.color (healthy health.balance) ] (text <| formatMoney health.balance)
            ]
        , wrappedRow summaryLine
            [ link [ Style.link ]
                { url = Route.url (Route.Account accountId Route.Bills)
                , label = text "Bills Total"
                }
            , row summaryDetail
                [ text "-", text <| formatMoney health.billsTotal ]
            ]
        , wrappedRow summaryLine
            [ link [ Style.link ]
                { url = Route.url (Route.Account accountId Route.Spending)
                , label = text <| "Spending"
                }
            , el [] (text <| formatMoney health.spendingDaily ++ "/day")
            , row summaryDetail
                [ text "-", text <| formatMoney health.spendingTotal ]
            ]
        , Components.maybe (advanceLine accountId "+") health.advance
        , row Style.banner
            [ el [] (text "Lowest Balance")
            , el [ alignRight, Font.color (healthy health.minimum) ] (text <| formatMoney health.minimum)
            ]

        -- load their actual income, so you have the ID
        , wrappedRow [ spacing 10, width fill ]
            [ link [ Style.link ]
                { url = Route.url <| Route.Account accountId <| Route.Budget Income paycheck.budgetId
                , label = text <| "Paycheck"
                }
            , el [ width (px 125) ] (text <| formatDate (scheduledDate health.paycheck))
            , el [ alignRight ] (text <| formatMoney paycheck.amount)
            ]
        , Components.maybe (advanceLine accountId "-") health.advance
        , row Style.banner
            [ el [] (text "Balance after paycheck")
            , el [ alignRight, Font.color (healthy health.afterPaycheck) ] (text <| formatMoney health.afterPaycheck)
            ]
        , row [] []
        ]


advanceLine : Id AccountId -> String -> Advance -> Element msg
advanceLine accountId sign adv =
    wrappedRow summaryLine
        [ link [ Style.link ]
            { url = Route.url (Route.Account accountId (Route.Advance adv.advanceId))
            , label = text "Advance"
            }
        , row summaryDetail
            [ text sign, text <| formatMoney adv.amount ]
        ]


summaryLine : List (Attribute msg)
summaryLine =
    [ spacing 10, width fill ]


summaryDetail : List (Attribute msg)
summaryDetail =
    [ alignRight, spacing 5 ]



-- summaryLine : String -> String -> Money -> Element msg
-- summaryLine label sign amount =
--     wrappedRow
--         [ el [] (text <| label)
--         , row
--             [ el [] (text sign)
--             , el [] (text <| formatMoney amount)
--             ]
--         ]
-- , row Style.banner
--     [ el [] (text "Coming up") ]
-- , column [ spacing 10, width fill ]
--     (List.map (viewEvent accountId) health.events)
-- , row [ spacing 10 ]
--     [ button (Style.button Style.secondary) { onPress = Just AddBill, label = text "Add Bill" }
--     , button (Style.button Style.secondary) { onPress = Just AddPaycheck, label = text "Add Paycheck" }
--     ]
-- budgets : Model -> Resource (List (BudgetId Budget))
-- budgets model =
--     Resource.map2 (\ps bs -> ps ++ bs) model.paychecks model.bills


healthy val =
    if Money.toCents val >= 0 then
        Style.dark

    else
        Style.lightRed



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
-- viewBill : Bill -> Element msg
-- viewBill bill =
--     column [ spacing 6, width fill, padding 10 ]
--         [ row [ spacing 8, width fill ]
--             [ text bill.budget.name
--             , el [ alignRight ] (text <| formatMoney bill.saved)
--             ]
--         , wrappedRow [ Font.size 16 ]
--             [ text <| formatMoney bill.budget.amount
--             , text " - "
--             , text <| formatDate bill.next
--             , text " - "
--             , text <| formatSchedule bill.budget.schedule
--             ]
--         ]
