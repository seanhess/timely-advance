module Page.Account exposing (Model, Msg, init, update, view)

import Debug
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Nimble.Api as Api exposing (Account, BankAccount, BankAccountType(..))
import Nimble.Style as Style


type alias Model =
    { accountId : String
    , account : Maybe Account
    , banks : List BankAccount
    , problems : List Problem
    }


type alias Problem =
    String


type Msg
    = LoadComplete (Result Http.Error Account)
    | LoadBanksComplete (Result Http.Error (List BankAccount))



-- TODO load the account


init : String -> ( Model, Cmd Msg )
init id =
    ( { accountId = id, account = Nothing, problems = [], banks = [] }
    , Cmd.batch
        [ Api.getAccountsById LoadComplete id
        , Api.getAccountsBanksById LoadBanksComplete id
        ]
    )



--Api.getAccounts LoadComplete )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadComplete (Err e) ->
            ( { model | problems = [ "Could not load account" ] ++ model.problems }, Cmd.none )

        LoadComplete (Ok acc) ->
            ( { model | account = Just acc }, Cmd.none )

        LoadBanksComplete (Err e) ->
            ( { model | problems = [ "Could not load banks" ] ++ model.problems }, Cmd.none )

        LoadBanksComplete (Ok bs) ->
            ( { model | banks = bs }, Cmd.none )


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Account")
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
