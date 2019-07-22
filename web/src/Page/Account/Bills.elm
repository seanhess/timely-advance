module Page.Account.Bills exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Http exposing (Error)
import List.Extra as List
import Page.Account.Budget exposing (formatSchedule)
import Platform.Updates exposing (Updates, command, modify, updates)
import Route
import Timely.Api as Api
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..), idValue)
import Timely.Types.Account exposing (AccountId)
import Timely.Types.AccountHealth exposing (AccountHealth)
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetInfo, BudgetType(..), Scheduled(..), scheduledDate, scheduledItem)
import Timely.Types.Date as Date exposing (Date, formatDate)
import Timely.Types.Money as Money exposing (Money, formatMoney)
import Timely.Types.Transactions exposing (Schedule(..))


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , bills : Resource (List Budget)
    , health : Resource AccountHealth
    }


type Msg
    = Back
    | OnBills (Result Http.Error (List Budget))
    | OnHealth (Result Http.Error AccountHealth)
    | AddBill
    | OnCreated (Result Http.Error (Id BudgetId))


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


upcomingBills : AccountHealth -> List Budget
upcomingBills health =
    List.map scheduledItem health.bills
        |> List.uniqueBy (.budgetId >> idValue)


otherBills : List Budget -> List Budget -> List Budget
otherBills upcoming bills =
    List.filter (not << isUpcoming upcoming) bills


isUpcoming : List Budget -> Budget -> Bool
isUpcoming upcoming bill =
    List.member bill upcoming


upcomingBillsByDate : AccountHealth -> List ( Date, List Budget )
upcomingBillsByDate health =
    []



-- TODO a resource or a map or something that gives me otherBills and Scheduled Bills (upcoming)


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        isBudget : Budget -> Budget -> Bool
        isBudget b bi =
            b.budgetId == bi.budgetId

        findBudget : Budget -> List Budget -> Maybe Budget
        findBudget bud bs =
            bs
                |> List.filter (isBudget bud)
                |> List.head

        goBudget : BudgetType -> Budget -> Cmd Msg
        goBudget typ b =
            goBudgetId typ b.budgetId

        goBudgetId : BudgetType -> Id BudgetId -> Cmd Msg
        goBudgetId typ bid =
            Route.pushUrl model.key (Route.Account model.accountId (Route.Budget typ bid))

        -- navBudget : BudgetType -> Resource (List (BudgetId Budget)) -> Event -> Cmd Msg
        -- navBudget bt rs event =
        --     rs
        --         |> Resource.toMaybe
        --         |> Maybe.andThen (findBudget event.budget)
        --         |> Maybe.map (goBudget bt)
        --         |> Maybe.withDefault Cmd.none
        defaultBudget : String -> BudgetInfo
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
    let
        other health bills =
            otherBills (upcomingBills health) bills
    in
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Back
                , paragraph [] [ text "Here are your bills" ]
                ]
            ]
        , column Style.section
            [ resource (viewUpcomingBills model.accountId) (Resource.map .bills model.health)
            , resource (viewOtherBills model.accountId) (Resource.map2 other model.health model.bills)
            , button (Style.button Style.secondary) { onPress = Just AddBill, label = Style.label "Add a Bill" }
            , row [] []
            ]
        ]


viewUpcomingBills : Id AccountId -> List (Scheduled Budget) -> Element Msg
viewUpcomingBills accountId bills =
    column [ spacing 10, width fill ]
        [ el Style.banner (text "Bills due by next paycheck")
        , column [ width fill ]
            (List.map (\( d, bs ) -> viewUpcomingByDate accountId d bs) <| billsGroupedByDate bills)
        ]


billsGroupedByDate : List (Scheduled Budget) -> List ( Date, List Budget )
billsGroupedByDate sbs =
    let
        isDate s1 s2 =
            scheduledDate s1 == scheduledDate s2

        dateGroup ( s, ss ) =
            ( scheduledDate s, scheduledItem s :: List.map scheduledItem ss )
    in
    List.groupWhile isDate sbs
        |> List.map dateGroup


viewUpcomingByDate : Id AccountId -> Date -> List Budget -> Element Msg
viewUpcomingByDate accountId date bills =
    column [ spacing 10, width fill ]
        [ el [ width fill, padding 6, Background.color Style.lightGray, Border.widthXY 0 1, Border.color Style.gray ] (text <| formatDate date)
        , column [ spacing 10, width fill ]
            (List.map (viewBill accountId) bills)
        ]


viewUpcomingBill : Id AccountId -> Scheduled Budget -> Element Msg
viewUpcomingBill accountId s =
    let
        bill =
            scheduledItem s

        date =
            scheduledDate s
    in
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


viewOtherBills : Id AccountId -> List Budget -> Element Msg
viewOtherBills accountId bills =
    column [ spacing 10, width fill ]
        [ el Style.banner (text "Other Bills")
        , column [ width fill ]
            (List.map (viewBill accountId) bills)
        ]


viewBill : Id AccountId -> Budget -> Element Msg
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
