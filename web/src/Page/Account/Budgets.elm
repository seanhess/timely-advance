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
import Platform.Updates exposing (Updates, command, updates)
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (AccountId, Id(..))
import Timely.Components as Components
import Timely.Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types.Money exposing (formatMoney)
import Timely.Types.Transactions exposing (Group, History, Schedule(..), formatBiweek, formatWeekday)
import Validate exposing (Validator, validate)


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , history : Resource History
    , income : Maybe Group
    , bills : List Group
    }


type Msg
    = OnBack
    | OnHistory (Result Http.Error History)
    | OnIncome Group Bool
    | OnBill Group Bool
    | Save


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key id =
    ( { key = key
      , accountId = id
      , history = Loading
      , income = Nothing
      , bills = []
      }
    , Api.getTransactionHistory OnHistory id
    )


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    case msg of
        OnBack ->
            updates model
                |> command (Route.pushUrl model.key <| Route.Account model.accountId Route.AccountMain)

        OnHistory (Err e) ->
            updates { model | history = Failed e }

        OnHistory (Ok h) ->
            updates { model | history = Ready h }

        OnIncome inc selected ->
            let
                income =
                    if selected then
                        Just inc

                    else
                        Nothing
            in
            updates { model | income = income }

        OnBill bill selected ->
            let
                delete g =
                    List.filter (\a -> a /= g)

                bills =
                    if selected then
                        bill :: model.bills

                    else
                        delete bill model.bills
            in
            updates { model | bills = bills }

        Save ->
            case validate model of
                Err _ ->
                    updates model

                Ok ( inc, bills ) ->
                    updates model
                        |> command (Route.pushUrl model.key <| Route.Account model.accountId Route.AccountMain)


view : Model -> Element Msg
view model =
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
            [ resource (viewHistory model) model.history
            , submitButton Save isValid (text "Save")
            ]
        ]


viewHistory : Model -> History -> Element Msg
viewHistory model history =
    let
        isIncome group =
            Just group == model.income

        isExpense group =
            List.member group model.bills
    in
    column [ spacing 15, width fill ]
        [ el Style.banner (text "Income")
        , paragraph [] [ text "Select your primary income" ]
        , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] (List.map (viewGroup OnIncome isIncome) history.income)
        , el Style.banner (text "Bills")
        , paragraph [] [ text "Select all your bills" ]
        , column [ spacing 0, width fill, Border.widthXY 0 1, Border.color Style.gray ] (List.map (viewGroup OnBill isExpense) history.expenses)
        ]



-- wait, we don't know
-- it depends on whether group == model.group


viewGroup : (Group -> Bool -> Msg) -> (Group -> Bool) -> Group -> Element Msg
viewGroup onSelect isSelected group =
    row [ paddingXY 0 10, Border.widthXY 0 1, Border.color Style.gray, width fill, spacing 14 ]
        [ select (onSelect group) (isSelected group)
        , column [ spacing 6, width fill ]
            [ row [ spacing 8, width fill ]
                [ text group.name
                , el [ alignRight ] (text (formatMoney group.average))
                ]
            , row [ Font.size 16 ] [ text (formatSchedule group.schedule) ]
            ]
        ]


formatSchedule : Schedule -> String
formatSchedule schedule =
    let
        formatDay n =
            String.fromInt n ++ "th"
    in
    case schedule of
        Weekly info ->
            "Weekly on " ++ formatWeekday info.weekday

        Biweekly info ->
            "Biweekly on " ++ formatWeekday info.weekday ++ " " ++ formatBiweek info.bi

        Monthly info ->
            "Monthly on " ++ formatDay info.date

        Semimonthly info ->
            "Semimonthly on " ++ formatDay info.date1 ++ " " ++ formatDay info.date2


groupKey : Group -> String
groupKey group =
    String.join "-" [ group.name, formatMoney group.average ]


validate : Model -> Result Invalid ( Group, List Group )
validate model =
    let
        validIncome mi =
            case mi of
                Nothing ->
                    Err NoIncome

                Just i ->
                    Ok i

        validBills bills =
            case bills of
                [] ->
                    Err NoBills

                bs ->
                    Ok bs
    in
    Result.map2 (\i b -> ( i, b ))
        (validIncome model.income)
        (validBills model.bills)


type Invalid
    = NoIncome
    | NoBills



-- Select -------------------------------


select : (Bool -> msg) -> Bool -> Element msg
select onSelect selected =
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
