module Page.Account exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import Http exposing (Error)
import Page.Account.Breakdown as Breakdown
import Route
import Timely.Api as Api exposing (Account, AccountId, Advance, BankAccount, BankAccountType(..), Customer, advanceIsActive, advanceIsCollected, advanceIsOffer)
import Timely.Resource as Resource exposing (Resource(..), resource, resource_)
import Timely.Style as Style
import Timely.Types exposing (Id, idValue)
import Timely.Types.AccountHealth exposing (AccountHealth)
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
    }


type Msg
    = OnAccount (Result Error Account)
    | OnCustomer (Result Error Customer)
    | OnHealth (Result Error AccountHealth)
    | OnBanks (Result Error (List BankAccount))
    | OnTransactions (Result Error (List TransRow))
    | OnAdvances (Result Error (List Advance))
    | Logout
    | LogoutDone (Result Error ())


init : Id AccountId -> ( Model, Cmd Msg )
init id =
    ( { accountId = id
      , customer = Loading
      , account = Loading
      , health = Loading
      , banks = Loading
      , advances = Loading
      , transactions = Loading
      }
    , Cmd.batch
        [ Api.getAccount OnAccount id
        , Api.getAccountHealth OnHealth id
        , Api.getCustomer OnCustomer id
        , Api.getAccountBanks OnBanks id
        , Api.getAdvances OnAdvances id
        , Api.getTransactions OnTransactions id
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update nav msg model =
    case msg of
        OnAccount ra ->
            ( { model | account = Resource.fromResult ra }, Cmd.none )

        OnHealth rh ->
            ( { model | health = Resource.fromResult rh }, Cmd.none )

        OnCustomer rc ->
            ( { model | customer = Resource.fromResult rc }, Cmd.none )

        OnTransactions rt ->
            ( { model | transactions = Resource.fromResult rt }, Cmd.none )

        OnBanks rb ->
            ( { model | banks = Resource.fromResult rb }, Cmd.none )

        OnAdvances ra ->
            ( { model | advances = Resource.fromResult ra }, Cmd.none )

        Logout ->
            ( model, Api.sessionsLogout LogoutDone )

        LogoutDone _ ->
            ( model, Nav.pushUrl nav (Route.url (Route.Onboard Route.Landing)) )


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
                , row [ width fill ] []
                , Input.button [ Style.link ] { onPress = Just Logout, label = text "Logout" }
                ]
            , resource (offersView model.accountId) offers
            ]
        , column Style.section
            [ resource (accountHealth model.accountId) model.health
            , resource_
                (\_ -> Element.none)
                identity
                (Resource.pure accountInfo
                    |> Resource.apply model.account
                    |> Resource.apply model.health
                    |> Resource.apply active
                )
            , resource (advancesView model.accountId) active
            , resource customerView <| model.customer
            , resource banksTable model.banks
            , resource transTable model.transactions
            , resource (advancesView model.accountId) collected

            -- , el Style.header (text "Advances")
            ]
        ]


accountHealth : Id AccountId -> AccountHealth -> Element Msg
accountHealth accountId health =
    let
        isHealthy =
            toCents health.projection.lowest >= 0

        healthyColor =
            if isHealthy then
                Style.green

            else
                Style.lightRed
    in
    link [ width fill ]
        { url = Route.url (Route.Account accountId Route.Breakdown)
        , label =
            column
                [ spacing 4
                , padding 20
                , width fill
                , Background.color healthyColor
                , Font.color Style.white
                , Style.box
                ]
                [ el [ Font.bold, centerX ] (text "Lowest Balance")
                , el [ Font.bold, Font.size 40, centerX ] (text <| formatMoney health.projection.lowest)
                ]
        }


accountInfo : Account -> AccountHealth -> List Advance -> Element Msg
accountInfo account health advances =
    column [ spacing 20 ]
        [ wrappedRow [ spacing 20 ]
            [ column [ spacing 4 ]
                [ el [ Font.bold ] (text "Balance")
                , el [] (text <| formatMoney health.projection.balance)
                ]
            , column [ spacing 4 ]
                [ el [ Font.bold ] (text "Lowest Balance")
                , el [] (text <| formatMoney health.projection.lowest)
                ]
            , column [ spacing 4 ]
                [ el [ Font.bold ] (text "Advances")
                , el [] (text <| (formatMoney <| Money.total <| List.map .amount advances))
                ]
            , column [ spacing 4 ]
                [ el [ Font.bold ] (text "Max Credit")
                , el [] (text <| formatMoney account.credit)
                ]
            ]
        ]


customerView : Customer -> Element Msg
customerView customer =
    Element.column [ spacing 20 ]
        [ wrappedRow [ spacing 10 ]
            [ Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "First Name")
                , el [] (text customer.firstName)
                ]
            , Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "Last Name")
                , el [] (text customer.lastName)
                ]
            , Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "Email")
                , el [] (text customer.email)
                ]
            ]
        ]


transTable : List TransRow -> Element Msg
transTable ts =
    Element.table [ spacing 8 ]
        { data = ts
        , columns =
            [ tableColumn "Amount" (\t -> text <| formatMoney t.amount)
            , tableColumn "Source" (\t -> text t.name)
            , tableColumn "Date" (\t -> text <| formatDate t.date)
            , tableColumn "Category" (\t -> text t.category)
            ]
        }


banksTable : List BankAccount -> Element Msg
banksTable banks =
    Element.table [ spacing 8 ]
        { data = banks
        , columns =
            [ tableColumn "Account" (\b -> text b.name)
            , tableColumn "Type" (\b -> text (accountType b.accountType))
            , tableColumn "Balance" (\b -> text <| formatMoney b.balance)
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
