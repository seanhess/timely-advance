module Page.Account.Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Date exposing (Unit(..))
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import Http exposing (Error)
import List.Extra as List
import Page.Account.Breakdown as Breakdown
import Platform.Updates exposing (Updates, command, updates)
import Route
import Timely.Api as Api exposing (Account, AccountId, BankAccount, BankAccountType(..), Customer, advanceIsActive, advanceIsCollected, advanceIsOffer)
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource, resource_)
import Timely.Style as Style
import Timely.Types exposing (Id, idValue)
import Timely.Types.AccountHealth exposing (AccountHealth)
import Timely.Types.Advance exposing (Advance)
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetType(..))
import Timely.Types.Daily as Daily exposing (DailyBalance)
import Timely.Types.Date as Date exposing (Date, formatDate)
import Timely.Types.Money as Money exposing (formatMoney, fromCents, toCents)
import Timely.Types.Transactions exposing (TransRow)


type alias Model =
    { accountId : Id AccountId
    , account : Resource Account
    , customer : Resource Customer
    , health : Resource AccountHealth
    , transactions : Resource (List TransRow)
    , banks : Resource (List BankAccount)
    , advances : Resource (List Advance)
    , paycheck : Resource Budget
    , now : Date
    }


type Msg
    = OnAccount (Result Error Account)
    | OnCustomer (Result Error Customer)
    | OnHealth (Result Error AccountHealth)
    | OnBanks (Result Error (List BankAccount))
    | OnTransactions (Result Error (List TransRow))
    | OnAdvances (Result Error (List Advance))
    | OnIncomes (Result Http.Error (List Budget))
    | Logout
    | LogoutDone (Result Error ())
    | OnDate Date


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key id =
    ( { accountId = id
      , customer = Loading
      , account = Loading
      , health = Loading
      , banks = Loading
      , advances = Loading
      , transactions = Loading
      , paycheck = Loading
      , now = Date.empty
      }
    , Cmd.batch
        [ Api.getAccount OnAccount id
        , Api.getAccountHealth OnHealth id
        , Api.getCustomer OnCustomer id
        , Api.getAccountBanks OnBanks id
        , Api.getAdvances OnAdvances id
        , Api.getTransactions OnTransactions id
        , Api.getIncomes OnIncomes id
        , Date.current OnDate
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update nav msg model =
    case msg of
        OnAccount ra ->
            updates { model | account = Resource.fromResult ra }
                |> command (Route.checkUnauthorized nav ra)

        OnHealth rh ->
            updates { model | health = Resource.fromResult rh }

        OnCustomer rc ->
            updates { model | customer = Resource.fromResult rc }

        OnTransactions rt ->
            updates { model | transactions = Resource.fromResult rt }

        OnBanks rb ->
            updates { model | banks = Resource.fromResult rb }

        OnAdvances ra ->
            updates { model | advances = Resource.fromResult ra }

        OnIncomes (Err e) ->
            updates { model | paycheck = Failed e }

        OnIncomes (Ok (i :: _)) ->
            updates { model | paycheck = Ready i }

        OnIncomes (Ok _) ->
            updates { model | paycheck = Failed (Http.BadBody "Missing income") }

        Logout ->
            updates model |> command (Api.sessionsLogout LogoutDone)

        LogoutDone _ ->
            updates model |> command (Route.goLanding nav)

        OnDate d ->
            updates { model | now = d }


view : Model -> Element Msg
view =
    viewMain


viewMain : Model -> Element Msg
viewMain model =
    let
        collected =
            Resource.map (List.filter advanceIsCollected) model.advances

        active =
            Resource.map (List.filter advanceIsActive) model.advances

        offers =
            Resource.map (List.filter advanceIsOffer) model.advances
    in
    column Style.page
        [ column Style.info
            [ row [ width fill ]
                [ el Style.header (text "Account")
                , column [ alignRight ]
                    [ Input.button [ Style.link ] { onPress = Just Logout, label = text "Logout" }
                    ]
                ]
            , resource (offersView model.accountId) offers
            ]
        , column Style.section
            [ resource (accountHealth model.accountId model.now) model.health
            , resource (advancesView model.accountId) collected
            , resource_
                (\_ -> Element.none)
                identity
                (Resource.pure (Breakdown.viewBreakdown model.accountId)
                    |> Resource.apply model.health
                    |> Resource.apply model.paycheck
                )

            -- , resource (advancesView model.accountId) active
            -- , el Style.header (text "Advances")
            ]
        ]


accountHealth : Id AccountId -> Date -> AccountHealth -> Element Msg
accountHealth accountId now health =
    let
        isHealthy amt =
            toCents amt >= 0

        healthyColor amt =
            if Money.toDollars amt < 0 then
                Style.lightRed

            else if Money.toDollars amt < 200 then
                Style.yellow

            else
                Style.green

        overdraftDaysMessage n =
            el [ centerX ] (text <| "Overdraft predicted in " ++ String.fromInt n ++ " days")

        overdraftDate : List DailyBalance -> Maybe Date
        overdraftDate dbs =
            List.find (\db -> toCents db.balance < 0) dbs
                |> Maybe.map (.daily >> .date)

        daysUntil start end =
            Date.diff Days start end
    in
    link [ width fill ]
        -- TODO should this do anything?
        { url = Route.url (Route.Account accountId Route.AccountMain)
        , label =
            column
                [ spacing 10
                , padding 14
                , width fill
                , Background.color (healthyColor health.minimum)
                , Font.color Style.white
                , Style.box
                ]
                [ el [ Font.bold, centerX ] (text "Predicted Lowest Balance")
                , el [ Font.bold, Font.size 40, centerX ] (text <| formatMoney health.minimum)
                , Components.maybe overdraftDaysMessage (Maybe.map (daysUntil now) <| overdraftDate health.dailyBalances)
                ]
        }


offersView : Id AccountId -> List Advance -> Element Msg
offersView accountId advances =
    Element.column [ spacing 10, width fill ]
        (List.map (offerView accountId) advances)


offerView : Id AccountId -> Advance -> Element Msg
offerView accountId advance =
    let
        advanceUrl =
            Route.url (Route.Account accountId (Route.Advance advance.advanceId))
    in
    Element.link
        [ width fill
        , Background.color Style.gray
        , Font.color Style.darkGreen
        , Style.box
        , padding 20
        ]
        { url = advanceUrl
        , label =
            Element.column [ width fill, spacing 8 ]
                [ el [ Font.bold, centerX, Style.link ] (text "Advance Offer")
                , el [ Font.bold, Font.size 40, centerX ] (text <| formatMoney advance.offer)
                ]
        }


tableColumn : String -> (a -> Element msg) -> Column a msg
tableColumn label vw =
    { header = el [ Font.bold ] (Element.text label)
    , width = fill
    , view = vw
    }


advanceLink : Id AccountId -> Advance -> Element Msg
advanceLink accountId advance =
    link [ Style.link ] { url = Route.url (Route.Account accountId (Route.Advance advance.advanceId)), label = text "view" }


advancesView : Id AccountId -> List Advance -> Element Msg
advancesView accountId advances =
    Element.column [ spacing 10, width fill ]
        (List.map (advanceView accountId) advances)


advanceView : Id AccountId -> Advance -> Element Msg
advanceView accountId advance =
    let
        advanceUrl =
            Route.url (Route.Account accountId (Route.Advance advance.advanceId))

        status a =
            case a.collected of
                Just c ->
                    "paid " ++ formatDate c

                Nothing ->
                    "due " ++ formatDate advance.due
    in
    wrappedRow
        [ padding 15
        , width fill
        , Background.color Style.gray
        , Font.color Style.dark
        , Style.box
        ]
        [ link [ Font.bold, Style.link, width fill ]
            { label = text "Advance", url = advanceUrl }
        , link []
            { label = text (formatMoney advance.amount ++ " " ++ status advance)
            , url = advanceUrl
            }
        ]



-- advanceView : Time.Zone -> Id AccountId -> Advance -> Element Msg
-- advanceView zone accountId advance =
--     Element.row [ spacing 10 ]
--         -- , Element.el [] (text <| "$" ++ formatDollars advance.offer)
--         [ advanceLink accountId advance
--         , Element.el [] (text <| "$" ++ formatDollars advance.amount)
--         , Element.el [] (text <| formatDate zone advance.offered)
--         , Element.el [] (text <| Maybe.withDefault "" <| Maybe.map (formatDate zone) advance.activated)
--         , Element.el [] (text <| formatDate zone advance.due)
--         -- , Element.el [] (text <| Debug.toString advance.offered)
--         -- , Element.el [] (text <| Debug.toString advance.activated)
--         -- , Element.el [] (text <| Debug.toString advance.collected)
--         ]


accountType : BankAccountType -> String
accountType t =
    case t of
        Checking ->
            "Checking"

        Savings ->
            "Savings"

        Credit ->
            "Credit"

        Other ->
            "Other"



-- the first negative date
-- daysUntilOverdraft : Date -> List DailyBalance -> Maybe Int
-- daysUntilOverdraft _ _ =
--     Just 5
