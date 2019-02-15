module Page.Account exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Http
import Route
import Time exposing (Zone)
import Timely.Api as Api exposing (Account, AccountId, Advance, BankAccount, BankAccountType(..), Customer, Id, advanceIsActive, advanceIsOffer, formatDate, formatDollars, idValue)
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style


type alias Model =
    { accountId : Id AccountId
    , account : Resource Account
    , banks : Resource (List BankAccount)
    , advances : Resource (List Advance)
    , zone : Zone
    }


type Msg
    = OnAccount (Result Http.Error Account)
    | OnBanks (Result Http.Error (List BankAccount))
    | OnAdvances (Result Http.Error (List Advance))
    | OnTimeZone Time.Zone
    | Logout
    | LogoutDone (Result Http.Error ())


init : Id AccountId -> ( Model, Cmd Msg )
init id =
    ( { accountId = id
      , account = Loading
      , banks = Loading
      , advances = Loading
      , zone = Time.utc
      }
    , Cmd.batch
        [ Api.getAccount OnAccount id
        , Api.getAccountBanks OnBanks id
        , Api.getAdvances OnAdvances id
        , Api.timezone OnTimeZone
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update nav msg model =
    case msg of
        OnAccount (Err e) ->
            ( { model | account = Failed e }, Cmd.none )

        OnAccount (Ok acc) ->
            ( { model | account = Ready acc }, Cmd.none )

        OnBanks (Err e) ->
            ( { model | banks = Failed e }, Cmd.none )

        OnBanks (Ok bs) ->
            ( { model | banks = Ready bs }, Cmd.none )

        OnAdvances (Err e) ->
            ( { model | advances = Failed e }, Cmd.none )

        OnAdvances (Ok adv) ->
            ( { model | advances = Ready adv }, Cmd.none )

        OnTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Logout ->
            ( model, Api.sessionsLogout LogoutDone )

        LogoutDone _ ->
            ( model, Nav.pushUrl nav (Route.url (Route.Onboard Route.Landing)) )


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ el Style.header (text "Account")
            , resource (offersView model.zone model.accountId) <| Resource.map (List.filter advanceIsOffer) model.advances
            ]
        , column Style.section
            [ resource accountHealth model.account
            , resource accountInfo model.account
            , resource (advancesView model.zone model.accountId) <| Resource.map (List.filter advanceIsActive) model.advances
            , resource customerView <| Resource.map .customer model.account
            , resource banksTable model.banks
            , Input.button [ Style.link ] { onPress = Just Logout, label = text "Logout" }

            -- , el Style.header (text "Advances")
            ]
        ]


accountHealth : Account -> Element Msg
accountHealth account =
    let
        projectedBalance =
            account.health.available - account.health.expenses

        isHealthy =
            projectedBalance > 0

        healthyColor =
            if isHealthy then
                Style.grey

            else
                Style.red
    in
    Element.column
        [ spacing 4
        , padding 20
        , width fill
        , Background.color Style.grey
        , Font.color healthyColor
        ]
        [ el [ Font.bold, centerX ] (text "Safe to Spend")
        , el [ Font.bold, Font.size 40, centerX ] (text <| "$" ++ formatDollars projectedBalance)
        ]


accountInfo : Account -> Element Msg
accountInfo account =
    wrappedRow [ spacing 20 ]
        [ column [ spacing 4 ]
            [ el [ Font.bold ] (text "Balance")
            , el [] (text <| "$" ++ formatDollars account.health.available)
            ]
        , column [ spacing 4 ]
            [ el [ Font.bold ] (text "Future Expenses")
            , el [] (text <| "$" ++ formatDollars account.health.expenses)
            ]
        , column [ spacing 4 ]
            [ el [ Font.bold ] (text "Max Credit")
            , el [] (text <| "$" ++ formatDollars account.credit)
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


banksTable : List BankAccount -> Element Msg
banksTable banks =
    Element.table []
        { data = banks
        , columns =
            [ tableColumn "Account" (\b -> text b.name)
            , tableColumn "Type" (\b -> text (accountType b.accountType))
            , tableColumn "Balance" (\b -> text <| "$" ++ formatDollars b.balance)
            ]
        }


advancesView : Time.Zone -> Id AccountId -> List Advance -> Element Msg
advancesView zone accountId advances =
    Element.column [ spacing 10, width fill ]
        (List.map (advanceView zone accountId) advances)


offersView : Time.Zone -> Id AccountId -> List Advance -> Element Msg
offersView zone accountId advances =
    Element.column [ spacing 10, width fill ]
        (List.map (offerView zone accountId) advances)


offerView : Time.Zone -> Id AccountId -> Advance -> Element Msg
offerView zone accountId advance =
    let
        advanceUrl =
            Route.url (Route.Account accountId (Route.Advance advance.advanceId))
    in
    Element.link
        [ width fill
        , Background.color Style.grey
        , Font.color Style.dark
        , padding 20
        ]
        { url = advanceUrl
        , label =
            Element.column [ width fill, spacing 8 ]
                [ el [ Font.bold, centerX, Style.link ] (text "Advance Offer")
                , el [ Font.bold, Font.size 40, centerX ] (text <| "$" ++ formatDollars advance.offer)
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


advanceView : Time.Zone -> Id AccountId -> Advance -> Element Msg
advanceView zone accountId advance =
    let
        advanceUrl =
            Route.url (Route.Account accountId (Route.Advance advance.advanceId))
    in
    wrappedRow
        [ padding 10
        , width fill
        , Background.color Style.grey
        , Font.color Style.dark
        ]
        [ link [ Font.bold, Style.link, width fill ]
            { label = text "Advance", url = advanceUrl }
        , link []
            { label = text ("$" ++ formatDollars advance.offer ++ " due " ++ formatDate zone advance.due)
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


viewError : String -> Http.Error -> Element Msg
viewError msg e =
    el [ Style.error ] (text <| msg ++ " " ++ Debug.toString e)
