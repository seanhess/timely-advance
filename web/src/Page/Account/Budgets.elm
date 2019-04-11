module Page.Account.Budgets exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Http
import List.Selection as Selection exposing (Selection)
import Platform.Updates exposing (Updates, command, updates)
import Result.Cat as Result
import Route
import Time exposing (Weekday(..))
import Timely.Api as Api exposing (AccountId, Id(..))
import Timely.Components as Components
import Timely.Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types.AccountHealth exposing (Budget)
import Timely.Types.Money exposing (formatMoney)
import Timely.Types.Transactions exposing (Group, History, Schedule(..), formatBiweek, formatDay, formatWeekday)
import Validate exposing (Validator, validate)



-- modify : (a -> Bool) -> (a -> a) -> List a -> List a
-- modify =


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , income : Selection Group
    , bills : Selection Group
    , editing : Maybe Group
    , savedBills : Bool
    , savedIncome : Bool
    }


type Msg
    = OnBack
    | OnHistory (Result Http.Error History)
    | Ignore (Result Http.Error String)
    | OnIncome Group Bool
    | OnBill Group Bool
    | OnEdit Group
    | OnEditSave Group
    | OnEditClose
    | Save
    | OnSavedIncome (Result Http.Error String)
    | OnSavedBills (Result Http.Error String)


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key id =
    ( { key = key
      , accountId = id
      , income = Selection.fromList []
      , bills = Selection.fromList []
      , editing = Nothing
      , savedBills = False
      , savedIncome = False
      }
    , Api.getTransactionHistory OnHistory id
    )


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        goAccount =
            Route.pushUrl model.key <| Route.Account model.accountId Route.AccountMain

        goAccountIfSaved mod =
            if mod.savedBills && mod.savedIncome then
                updates mod |> command goAccount

            else
                updates mod
    in
    case msg of
        OnBack ->
            updates model
                |> command goAccount

        Ignore _ ->
            updates model

        OnHistory (Err e) ->
            updates model

        OnHistory (Ok h) ->
            updates
                { model
                    | income = Selection.fromValues h.income
                    , bills = Selection.fromValues h.expenses
                }

        OnIncome inc selected ->
            updates { model | income = Selection.change inc model.income }

        OnBill bill selected ->
            updates { model | bills = Selection.set bill selected model.bills }

        OnEdit group ->
            updates { model | editing = Just group }

        OnEditClose ->
            updates { model | editing = Nothing }

        OnEditSave group ->
            updates
                { model
                    | editing = Nothing
                    , income = Selection.replace (isName group.name) group model.income
                    , bills = Selection.replace (isName group.name) group model.bills
                }

        Save ->
            case validate model of
                Err _ ->
                    updates model

                Ok ( inc, bs ) ->
                    updates model
                        |> command (Api.putSetIncome OnSavedIncome model.accountId inc)
                        |> command (Api.putSetExpenses OnSavedBills model.accountId bs)

        OnSavedIncome _ ->
            goAccountIfSaved { model | savedIncome = True }

        OnSavedBills _ ->
            goAccountIfSaved { model | savedBills = True }



-- we need to wait until both are finished
-- then we can go
-- |> command (Route.pushUrl model.key <| Route.Account model.accountId Route.AccountMain)


view : Model -> Element Msg
view model =
    case model.editing of
        Nothing ->
            viewMain model

        Just g ->
            viewEditing model g


viewEditing : Model -> Group -> Element Msg
viewEditing model group =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back OnEditClose
                , el Style.header (text <| "Budgets: " ++ group.name)
                ]
            ]
        , column Style.section
            [ el [ Font.bold ] (text "Schedule")
            , el [] (text (Maybe.withDefault "None" <| Maybe.map formatSchedule group.schedule))
            , selectSchedule group
            , button
                (Style.button Style.primary)
                { onPress = Just (OnEditSave group), label = text "Save" }
            ]
        ]


selectSchedule : Group -> Element Msg
selectSchedule group =
    column [ spacing 10 ]
        [ row [ spacing 10 ]
            [ button (Style.button Style.secondary) { onPress = Just (OnEdit { group | schedule = Just <| Monthly { date = 1 } }), label = text "Monthly" }
            , button (Style.button Style.secondary) { onPress = Just (OnEdit { group | schedule = Just <| Weekly { weekday = Mon } }), label = text "Weekly" }
            ]
        , case group.schedule of
            Just (Monthly _) ->
                wrappedRow [ spacing 2 ]
                    (List.map
                        (selectDate (\n -> OnEdit { group | schedule = Just <| Monthly { date = n } }))
                        (List.range 1 28)
                    )

            Just (Weekly _) ->
                wrappedRow [ spacing 2 ]
                    (List.map
                        (selectWeekday (\w -> OnEdit { group | schedule = Just <| Weekly { weekday = w } }))
                        [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
                    )

            _ ->
                none
        ]



-- (OnEdit { group | schedule = Just <| Monthly { date = n


selectDate : (Int -> Msg) -> Int -> Element Msg
selectDate onSelect n =
    button Style.option
        { onPress = Just (onSelect n)
        , label = el [ centerX ] (text (String.fromInt n))
        }


selectWeekday : (Weekday -> Msg) -> Weekday -> Element Msg
selectWeekday onSelect w =
    button Style.option
        { onPress = Just (onSelect w)
        , label = el [ centerX ] (text (formatWeekday w))
        }


viewMain : Model -> Element Msg
viewMain model =
    let
        isValid =
            case validate model of
                Err _ ->
                    False

                Ok _ ->
                    True
    in
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back OnBack
                , el Style.header (text "Budgets")
                ]
            ]
        , column Style.section
            [ viewHistory model
            , submitButton Save isValid (text "Save")
            ]
        ]


viewHistory : Model -> Element Msg
viewHistory model =
    let
        isIncome group =
            Selection.member group model.income

        isExpense group =
            Selection.member group model.bills
    in
    column [ spacing 15, width fill ]
        [ el Style.banner (text "Income")
        , paragraph [] [ text "Select your primary income" ]
        , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] (List.map (viewGroup OnIncome isIncome) <| Selection.toValues model.income)
        , el Style.banner (text "Bills")
        , paragraph [] [ text "Select all your bills" ]
        , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] (List.map (viewGroup OnBill isExpense) <| Selection.toValues model.bills)
        ]



-- wait, we don't know
-- it depends on whether group == model.group


viewGroup : (Group -> Bool -> Msg) -> (Group -> Bool) -> Group -> Element Msg
viewGroup onSelect isSelected group =
    row [ paddingXY 0 10, Border.widthXY 0 1, Border.color Style.gray, width fill, spacing 14 ]
        [ selectButton (onSelect group) (isSelected group)
        , column [ spacing 6, width fill ]
            [ row [ spacing 8, width fill ]
                [ text group.name
                , el [ alignRight ] (text (formatMoney group.average))
                ]
            , button [ Style.link ] { onPress = Just (OnEdit group), label = viewSchedule group.schedule }
            ]
        ]


viewSchedule : Maybe Schedule -> Element Msg
viewSchedule ms =
    row [ Font.size 16 ]
        [ text <|
            case ms of
                Nothing ->
                    "Select a schedule"

                Just s ->
                    formatSchedule s
        ]


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


validate : Model -> Result Invalid ( Budget, List Budget )
validate model =
    let
        validIncome mi =
            case Selection.get mi of
                [ i ] ->
                    Ok i

                _ ->
                    Err NoIncome

        validBills bills =
            case Selection.get bills of
                [] ->
                    Err NoBills

                bs ->
                    Ok bs
    in
    Result.map2 (\i b -> ( i, b ))
        (validIncome model.income |> Result.andThen validBudget)
        (validBills model.bills |> Result.andThen (Result.cat << List.map validBudget))


isName : String -> Group -> Bool
isName name g =
    g.name == name


findGroup : List Group -> String -> Maybe Group
findGroup groups name =
    List.head <| List.filter (isName name) groups


validBudget : Group -> Result Invalid Budget
validBudget group =
    case group.schedule of
        Nothing ->
            Err NoIncome

        Just s ->
            Ok
                { name = group.name
                , schedule = s
                , amount = group.average
                }


type Invalid
    = NoIncome
    | NoBills
    | NoHistory
    | NoGroup



-- Select -------------------------------


selectButton : (Bool -> msg) -> Bool -> Element msg
selectButton onSelect selected =
    let
        color =
            if selected then
                Style.darkBlue

            else
                Style.gray
    in
    button
        [ width (px 20), height (px 20), Background.color color ]
        { onPress = Just (onSelect (not selected))
        , label = none
        }



-- Disabled Button -------------------------------


submitButton : msg -> Bool -> Element msg -> Element msg
submitButton onMsg enabled label =
    let
        style =
            if enabled then
                Style.primary

            else
                Style.secondary

        action =
            if enabled then
                Just onMsg

            else
                Nothing
    in
    button (Style.button style)
        { onPress = action
        , label = label
        }
