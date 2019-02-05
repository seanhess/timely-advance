module Page.Account exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Route
import Time exposing (Zone)
import Timely.Api as Api exposing (Account, AccountId, Advance, BankAccount, BankAccountType(..), Id, advanceIsActive, advanceIsOffer, formatDate, formatDollars, idValue)
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
    Element.column Style.formPage
        [ el Style.header (text "Account")
        , Input.button [ Style.link ] { onPress = Just Logout, label = text "Logout" }
        , resource accountView model.account
        , el Style.header (text "Offer")
        , resource (advancesView model.zone model.accountId) <| Resource.map (List.filter advanceIsOffer) model.advances
        , el Style.header (text "Advances")
        , resource (advancesView model.zone model.accountId) <| Resource.map (List.filter advanceIsActive) model.advances
        , el Style.header (text "Banks")
        , resource banksTable model.banks
        ]


accountView : Account -> Element Msg
accountView account =
    Element.column [ spacing 20 ]
        [ Element.row [ spacing 10 ]
            [ Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "First Name")
                , el [] (text account.customer.firstName)
                ]
            , Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "Last Name")
                , el [] (text account.customer.lastName)
                ]
            , Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "Email")
                , el [] (text account.customer.email)
                ]
            ]
        , Element.row [ spacing 10 ]
            [ Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "Expenses")
                , el [] (text <| "$" ++ formatDollars account.health.expenses)
                ]
            , Element.column [ spacing 4 ]
                [ el [ Font.bold ] (text "Available")
                , el [] (text <| "$" ++ formatDollars account.health.available)
                ]
            ]
        , Element.column [ spacing 4 ]
            [ el [ Font.bold ] (text "Credit")
            , el [] (text <| "$" ++ formatDollars account.credit)
            ]
        ]


banksTable : List BankAccount -> Element Msg
banksTable banks =
    Element.table []
        { data = banks
        , columns =
            [ column "Name" (\b -> text b.name)
            , column "Type" (\b -> text (accountType b.accountType))
            , column "Balance" (\b -> text <| "$" ++ (formatDollars b.balance))
            ]
        }


advancesView : Time.Zone -> Id AccountId -> List Advance -> Element Msg
advancesView zone accountId advances =
    Element.column [ spacing 10 ]
        (List.map (advanceView zone accountId) advances)


column : String -> (a -> Element msg) -> Column a msg
column label vw =
    { header = el [ Font.bold ] (Element.text label)
    , width = fill
    , view = vw
    }


advanceLink : Id AccountId -> Advance -> Element Msg
advanceLink accountId advance =
    link [ Style.link ] { url = Route.url (Route.Account accountId (Route.Advance advance.advanceId)), label = text "view" }


advanceView : Time.Zone -> Id AccountId -> Advance -> Element Msg
advanceView zone accountId advance =
    Element.row [ spacing 10 ]
        -- , Element.el [] (text <| "$" ++ formatDollars advance.offer)
        [ advanceLink accountId advance
        , Element.el [] (text <| "$" ++ formatDollars advance.amount)
        , Element.el [] (text <| formatDate zone advance.offered)
        , Element.el [] (text <| Maybe.withDefault "" <| Maybe.map (formatDate zone) advance.activated)
        , Element.el [] (text <| formatDate zone advance.due)

        -- , Element.el [] (text <| Debug.toString advance.offered)
        -- , Element.el [] (text <| Debug.toString advance.activated)
        -- , Element.el [] (text <| Debug.toString advance.collected)
        ]


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
