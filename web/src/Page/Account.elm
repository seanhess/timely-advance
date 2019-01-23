module Page.Account exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Route
import Timely.Api as Api exposing (Account, BankAccount, BankAccountType(..), Id)
import Timely.Style as Style


type alias Model =
    { accountId : Id Account
    , account : Maybe Account
    , banks : List BankAccount
    , status : Status
    }


type Status
    = Ready
    | Loading
    | Failed Problem Http.Error


type alias Problem =
    String


type Msg
    = LoadComplete (Result Http.Error Account)
    | LoadBanksComplete (Result Http.Error (List BankAccount))
    | Logout
    | LogoutDone (Result Http.Error ())


init : Id Account -> ( Model, Cmd Msg )
init id =
    ( { accountId = id, account = Nothing, status = Loading, banks = [] }
    , Cmd.batch
        [ Api.getAccount LoadComplete
        , Api.getAccountBanks LoadBanksComplete
        ]
    )


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update nav msg model =
    case msg of
        LoadComplete (Err e) ->
            ( { model | status = Failed "Could not load account" e }, Cmd.none )

        LoadComplete (Ok acc) ->
            ( { model | status = Ready, account = Just acc }, Cmd.none )

        LoadBanksComplete (Err e) ->
            ( { model | status = Failed "Could not load banks" e }, Cmd.none )

        LoadBanksComplete (Ok bs) ->
            ( { model | status = Ready, banks = bs }, Cmd.none )

        Logout ->
            ( model, Api.sessionsLogout LogoutDone )

        LogoutDone _ ->
            ( model, Nav.pushUrl nav (Route.url (Route.Onboard Route.Landing)) )


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Account")
        , Input.button [] { onPress = Just Logout, label = text "Logout" }
        , accountView model.account
        , el Style.header (text "Banks")
        , banksTable model.banks
        ]


accountView : Maybe Account -> Element Msg
accountView ma =
    case ma of
        Nothing ->
            Element.row [] []

        Just account ->
            Element.row [ spacing 8, padding 4 ]
                [ Element.column []
                    [ el [ Font.bold ] (text "First Name")
                    , el [] (text account.customer.firstName)
                    ]
                , el [] (text "Last Name")
                , el [ Font.bold ] (text "Last Name")
                , el [] (text account.customer.lastName)
                , el [ Font.bold ] (text "Email")
                , el [] (text account.customer.email)
                ]


banksTable : List BankAccount -> Element Msg
banksTable banks =
    Element.table []
        { data = banks
        , columns =
            [ { header = el [ Font.bold ] (Element.text "Name")
              , width = fill
              , view = \b -> Element.text b.name
              }
            , { header = el [ Font.bold ] (Element.text "Type")
              , width = fill
              , view = \b -> Element.text (accountType b.accountType)
              }
            , { header = el [ Font.bold ] (Element.text "Balance")
              , width = fill
              , view = \b -> Element.text (Debug.toString b.balance)
              }
            ]
        }


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
