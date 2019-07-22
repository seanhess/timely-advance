module Page.Account.Budget exposing (Model, Msg(..), formatSchedule, init, update, view, viewSchedule, viewTransaction)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Html.Attributes as Html
import Http
import List.Selection as Selection exposing (Selection)
import Platform.Updates exposing (Updates, command, modify, updates)
import Result.Cat as Result
import Route
import Time exposing (Weekday(..))
import Timely.Api as Api
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.Account exposing (AccountId)
import Timely.Types.Budget as Budget exposing (Budget, BudgetId, BudgetInfo, BudgetType(..), info)
import Timely.Types.Date as Date exposing (Date, formatDate)
import Timely.Types.Money as Money exposing (Money, formatMoney, formatMoneyNoSign)
import Timely.Types.Transactions exposing (Group, History, Schedule(..), Transaction, formatBiweek, formatDay, formatWeekday, isMonthly, isWeekly)
import Validate exposing (Validator, validate)


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , budgetId : Id BudgetId
    , budgetType : BudgetType
    , budget : Resource BudgetInfo

    -- needs to be a string or it gets really confused
    , name : String
    , schedule : Schedule
    , amount : String
    }


type Msg
    = Close
    | OnBudgets (Result Http.Error (List Budget))
    | OnDone (Result Http.Error String)
    | Save
    | Delete
    | EditAmount String
    | EditSchedule Schedule
    | EditName String


init : Nav.Key -> Id AccountId -> BudgetType -> Id BudgetId -> ( Model, Cmd Msg )
init key accountId budgetType budgetId =
    ( { key = key
      , accountId = accountId
      , budgetId = budgetId
      , budgetType = budgetType
      , budget = Loading
      , amount = ""
      , name = ""
      , schedule = Monthly { date = 1 }
      }
    , case budgetType of
        Income ->
            Api.getIncomes OnBudgets accountId

        Expense ->
            Api.getExpenses OnBudgets accountId
    )


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        goBack =
            Route.pushUrl model.key <|
                Route.Account model.accountId <|
                    case model.budgetType of
                        Income ->
                            Route.AccountMain

                        Expense ->
                            Route.Bills

        isBudget b =
            b.budgetId == model.budgetId
    in
    case msg of
        Close ->
            updates model
                |> command goBack

        OnBudgets (Err e) ->
            updates { model | budget = Failed e }
                |> command (Route.checkUnauthorized model.key (Err e))

        OnBudgets (Ok bs) ->
            case List.filter isBudget bs of
                [ b ] ->
                    updates { model | budget = Ready (Budget.info b), name = b.name, schedule = b.schedule, amount = formatMoneyNoSign b.amount }

                _ ->
                    updates model

        Save ->
            let
                save =
                    case validBudget model of
                        Nothing ->
                            Cmd.none

                        Just b ->
                            Api.putBudget model.budgetType OnDone model.accountId model.budgetId b
            in
            updates model
                |> command save

        OnDone _ ->
            updates model
                |> command goBack

        EditAmount s ->
            updates { model | amount = s }

        EditName n ->
            updates { model | name = n }

        EditSchedule s ->
            updates { model | schedule = s }

        Delete ->
            updates model
                |> command (Api.delBudget model.budgetType OnDone model.accountId model.budgetId)


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Close
                , paragraph [] [ viewEditMessage model.budgetType ]
                ]
            ]
        , column Style.section
            [ resource (viewBudget model) model.budget
            , viewActions
            ]
        ]


viewPopup : Model -> Element Msg
viewPopup model =
    -- column Style.page
    column Style.section
        [ row [ spacing 15, width fill ]
            [ viewEditMessage model.budgetType
            , el [ alignRight ] (Components.close Close)
            ]
        , resource (viewBudget model) model.budget
        , viewActions
        ]


viewActions : Element Msg
viewActions =
    row [ spacing 10, width fill ]
        [ button (Style.button Style.primary)
            { onPress = Just Save, label = el [] (Style.label "Save") }
        , button (Style.button Style.destroy)
            { onPress = Just Delete, label = el [] (Style.label "Delete") }
        ]


viewEditMessage : BudgetType -> Element msg
viewEditMessage typ =
    text <| "Edit your " ++ formatBudgetType typ


viewBudget : Model -> BudgetInfo -> Element Msg
viewBudget model budget =
    column [ spacing 10, width fill ]
        [ fieldName model.name
        , fieldAmount model.amount
        , selectSchedule model.schedule
        ]


fieldAmount : String -> Element Msg
fieldAmount amount =
    Input.text [ htmlAttribute (Html.type_ "number") ]
        { text = amount
        , placeholder = Nothing
        , onChange = EditAmount
        , label = Input.labelAbove [ Font.size 14 ] (text "Amount (USD)")
        }


fieldName : String -> Element Msg
fieldName name =
    Input.text []
        { text = name
        , placeholder = Nothing
        , onChange = EditName
        , label = Input.labelAbove [ Font.size 14 ] (text "Name")
        }


selectSchedule : Schedule -> Element Msg
selectSchedule schedule =
    column [ spacing 10 ]
        [ row [ spacing 10 ]
            [ Components.option (EditSchedule (Monthly { date = 1 })) (isMonthly schedule) (text "Monthly")
            , Components.option (EditSchedule (Weekly { weekday = Mon })) (isWeekly schedule) (text "Weekly")
            ]
        , case schedule of
            Monthly { date } ->
                wrappedRow [ spacing 2 ] <|
                    List.map
                        (selectDate (\n -> EditSchedule (Monthly { date = n })) date)
                        (List.range 1 28)

            Weekly { weekday } ->
                wrappedRow [ spacing 2 ] <|
                    List.map
                        (selectWeekday (\w -> EditSchedule (Weekly { weekday = w })) weekday)
                        [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]

            _ ->
                none
        ]


selectDate : (Int -> Msg) -> Int -> Int -> Element Msg
selectDate onSelect sn n =
    Components.option
        (onSelect n)
        (sn == n)
        (el [ centerX ] (text (String.fromInt n)))


selectWeekday : (Weekday -> Msg) -> Weekday -> Weekday -> Element Msg
selectWeekday onSelect sw w =
    Components.option
        (onSelect w)
        (sw == w)
        (el [ centerX ] (text (formatWeekday w)))


formatBudgetType : BudgetType -> String
formatBudgetType bt =
    case bt of
        Income ->
            "Paycheck"

        Expense ->
            "Bill"



-- budgets : Model -> Resource ( List Budget, List Budget )
-- budgets model =
--     Resource.map2 (\ps bs -> ( ps, bs )) model.paychecks model.bills
-- viewBudgets : ( List Budget, List Budget ) -> Element Msg
-- viewBudgets ( paychecks, bills ) =
--     column [ spacing 15, width fill ]
--         [ el Style.banner (text "Income")
--         , paragraph [] [ text "Select your primary income" ]
--         , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] (List.map (viewBudget OnSelectIncome) paychecks)
--         , el Style.banner (text "Bills")
--         , paragraph [] [ text "Select all your bills" ]
--         , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] (List.map (viewBudget OnSelectBill) bills)
--         ]
-- wait, we don't know
-- it depends on whether group == model.group
-- viewBudget : (Budget -> Msg) -> Budget -> Element Msg
-- viewBudget onSelect budget =
--     row [ paddingXY 0 10, Border.widthXY 0 1, Border.color Style.gray, width fill, spacing 14 ]
--         [ column [ spacing 6, width fill ]
--             [ row [ spacing 8, width fill ]
--                 [ text budget.name
--                 , el [ alignRight ] (text (formatMoney budget.amount))
--                 ]
--             , row [ width fill ]
--                 [ button [ Style.link ] { onPress = Just (onSelect budget), label = viewSchedule budget.schedule }
--                 -- , wrappedRow [ spacing 4, alignRight ] (List.map viewTransaction (List.take 1 budget.transactions))
--                 ]
--             ]
--         ]


viewSchedule : Schedule -> Element Msg
viewSchedule s =
    row [ Font.size 16 ]
        [ text <| formatSchedule s ]


viewTransaction : Transaction -> Element Msg
viewTransaction t =
    el [ Font.size 14 ] (text <| formatDate t.date)


formatSchedule : Schedule -> String
formatSchedule schedule =
    case schedule of
        Weekly info ->
            "Weekly on " ++ formatWeekday info.weekday

        Biweekly info ->
            "Biweekly on " ++ formatWeekday info.weekday ++ " " ++ formatBiweek info.bi

        Monthly info ->
            "Monthly on " ++ formatDay info.date

        Semimonthly info ->
            "Semimonthly on " ++ formatDay info.date1 ++ " " ++ formatDay info.date2


validAmount : String -> Maybe Money
validAmount value =
    Maybe.map (Money.fromDollars << abs) (String.toFloat value)


validBudget : Model -> Maybe BudgetInfo
validBudget model =
    validAmount model.amount
        |> Maybe.map (BudgetInfo model.name model.schedule)
