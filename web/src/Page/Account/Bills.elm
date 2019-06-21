module Page.Account.Bills exposing (Model, Msg(..), init, update, view)

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
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetType(..), Scheduled)
import Timely.Types.Date as Date exposing (Date, formatDate)
import Timely.Types.Money as Money exposing (Money, formatMoney)
import Timely.Types.Transactions exposing (Schedule(..))


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , bills : Resource (List (BudgetId Budget))
    , health : Resource AccountHealth
    }


type Msg
    = Back
    | OnBills (Result Http.Error (List (BudgetId Budget)))
    | OnHealth (Result Http.Error AccountHealth)
    | AddBill
    | OnCreated (Result Http.Error (Id Budget))


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { key = key
      , accountId = accountId
      , bills = Loading
      , health = Loading
      }
    , Cmd.batch
        [ Api.getExpenses OnBills accountId
        , Api.getAccountHealth OnHealth accountId
        ]
    )


upcomingBills : AccountHealth -> List (Scheduled (BudgetId Budget))
upcomingBills health =
    health.bills



-- otherBills : Model -> List (BudgetId Budget))
-- otherBills = _


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        isBudget : Budget -> BudgetId Budget -> Bool
        isBudget b bi =
            (b.name == bi.name)
                && (b.schedule == bi.schedule)
                && (b.amount == bi.amount)

        findBudget : Budget -> List (BudgetId Budget) -> Maybe (BudgetId Budget)
        findBudget bud bs =
            bs
                |> List.filter (isBudget bud)
                |> List.head

        goBudget : BudgetType -> BudgetId Budget -> Cmd Msg
        goBudget typ b =
            goBudgetId typ b.budgetId

        goBudgetId : BudgetType -> Id Budget -> Cmd Msg
        goBudgetId typ bid =
            Route.pushUrl model.key (Route.Account model.accountId (Route.Budget typ bid))

        -- navBudget : BudgetType -> Resource (List (BudgetId Budget)) -> Event -> Cmd Msg
        -- navBudget bt rs event =
        --     rs
        --         |> Resource.toMaybe
        --         |> Maybe.andThen (findBudget event.budget)
        --         |> Maybe.map (goBudget bt)
        --         |> Maybe.withDefault Cmd.none
        defaultBudget : String -> Budget
        defaultBudget name =
            { name = name
            , schedule = Monthly { date = 1 }
            , amount = Money.fromCents 0
            }
    in
    case msg of
        Back ->
            updates model
                |> command (Route.goAccount model.key model.accountId)

        OnBills bs ->
            updates { model | bills = Resource.fromResult bs }

        OnHealth r ->
            updates { model | health = Resource.fromResult r }

        -- Select e ->
        --     updates model
        --         |> command (navBudget Income model.paychecks e)
        --         |> command (navBudget Expense model.bills e)
        AddBill ->
            updates model
                |> command (Api.createExpense OnCreated model.accountId (defaultBudget "New Bill"))

        OnCreated (Ok budgetId) ->
            updates model
                |> command (goBudgetId Expense budgetId)

        OnCreated _ ->
            updates model


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Back
                , paragraph [] [ text "Here are your bills" ]
                ]
            ]
        , column Style.section
            [ resource (viewBills model.accountId) model.bills
            ]
        ]


viewBills : Id AccountId -> List (BudgetId Budget) -> Element Msg
viewBills accountId bills =
    column [ spacing 10, width fill ]
        [ el Style.banner (text "Bills")
        , column [ width fill ]
            (List.map (viewBill accountId) bills)
        , button (Style.button Style.secondary) { onPress = Just AddBill, label = text "Add a Bill" }
        , row [] []
        ]


viewBill : Id AccountId -> BudgetId Budget -> Element Msg
viewBill accountId bill =
    wrappedRow [ spacing 6, width fill, padding 10 ]
        [ column [ spacing 6 ]
            [ link [ Style.link ]
                { url = Route.url <| Route.Account accountId (Route.Budget Expense bill.budgetId)
                , label = text bill.name
                }
            , el [ Font.size 16 ]
                (text <| formatSchedule bill.schedule)
            ]
        , el [ alignRight ] (text <| formatMoney bill.amount)
        ]
