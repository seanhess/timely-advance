module Timely.Api exposing (AccountCustomer, Amount, Approval, ApprovalResult(..), BankAccount, BankAccountType(..), Customer, Denial, Session, advanceIsActive, advanceIsCollected, advanceIsOffer, createExpense, createIncome, delBudget, delExpense, delIncome, delSubscription, deleteAccount, editExpense, editIncome, expectId, getAccount, getAccountBanks, getAccountHealth, getAdvance, getAdvances, getApplication, getApplicationResult, getAvailableSubscriptions, getCustomer, getCustomers, getExpenses, getIncomes, getSubscription, getTransactionHistory, getTransactions, postAdvanceAccept, postApplications, putBudget, putSpending, putSubscription, request, requestGET, requestPOST, sessionsAuthAdmin, sessionsCheckCode, sessionsCreateCode, sessionsGet, sessionsLogout, usedCredit)

import Http exposing (Error, Expect)
import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String
import Task
import Time exposing (Month(..))
import Timely.Types exposing (Auth, AuthCode, Id(..), Phone, Token, Valid(..), decodeId, decodeValid, encodeId, encodeValid, idValue)
import Timely.Types.Account exposing (Account, AccountId)
import Timely.Types.AccountHealth exposing (AccountHealth, decodeAccountHealth)
import Timely.Types.Advance exposing (Advance, AdvanceId, decodeAdvance)
import Timely.Types.Application as Application exposing (AccountInfo, Application, SSN)
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetInfo, BudgetType(..), decodeBudget, decodeBudgetInfo, encodeBudget)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney, fromCents, toCents)
import Timely.Types.Subscription as Subscription exposing (Subscription)
import Timely.Types.Transactions exposing (History, Schedule, TransRow, Transaction, decodeHistory, decodeSchedule, decodeTransRow, decodeTransaction, encodeSchedule)


type alias Session =
    { phone : Phone
    , accountId : Maybe (Id AccountId)
    , isAdmin : Bool
    }


type alias BankAccount =
    { accountId : String
    , accountType : BankAccountType
    , name : String
    , balance : Money
    }


type BankAccountType
    = Checking
    | Savings
    | Credit
    | Other


type alias Customer =
    { accountId : Id AccountId
    , firstName : String
    , middleName : Maybe String
    , lastName : String
    , email : String
    }


type alias AccountCustomer =
    { account : Account
    , customer : Customer
    }


type ApprovalResult
    = Approved Approval
    | Denied Denial


type alias Approval =
    { approvalAmount : Int
    }


type alias Denial =
    { denial : String
    }


type alias Amount =
    { amount : Money
    }



-- |> required "budgetId" decodeId


encodeAmount : Amount -> Encode.Value
encodeAmount x =
    Encode.object
        [ ( "amount", encodeMoney x.amount )
        ]


decodeApprovalResult : Decoder ApprovalResult
decodeApprovalResult =
    Decode.oneOf
        [ Decode.map Denied decodeDenial
        , Decode.map Approved decodeApproval
        ]


decodeDenial : Decoder Denial
decodeDenial =
    Decode.succeed Denial
        |> required "denial" string


decodeApproval : Decoder Approval
decodeApproval =
    Decode.succeed Approval
        |> required "approvalAmount" int


decodeAccount : Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "accountId" decodeId
        |> required "phone" string



-- type alias Transaction =
--     { transactionId : String
--     , date : Date
--     , category : String
--     , pending : Bool
--     , amount : Money
--     , name : String
--     }


decodeBankAccount : Decoder BankAccount
decodeBankAccount =
    Decode.succeed BankAccount
        |> required "accountId" string
        |> required "accountType" decodeBankAccountType
        |> required "name" string
        |> required "balance" decodeMoney


decodeAccountCustomer : Decoder AccountCustomer
decodeAccountCustomer =
    Decode.succeed AccountCustomer
        |> required "account" decodeAccount
        |> required "customer" decodeCustomer


decodeCustomer : Decoder Customer
decodeCustomer =
    Decode.succeed Customer
        |> required "accountId" decodeId
        |> required "firstName" string
        |> required "middleName" (nullable string)
        |> required "lastName" string
        |> required "email" string


decodeSession : Decoder Session
decodeSession =
    Decode.succeed Session
        |> required "phone" decodeId
        |> required "accountId" (nullable decodeId)
        |> required "isAdmin" bool


decodeBankAccountType : Decoder BankAccountType
decodeBankAccountType =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Checking" ->
                        Decode.succeed Checking

                    "Savings" ->
                        Decode.succeed Savings

                    "Credit" ->
                        Decode.succeed Credit

                    "Other" ->
                        Decode.succeed Other

                    _ ->
                        Decode.fail ("Invalid BankAccountType, " ++ string)
            )


request : String -> Http.Body -> (Result Error a -> msg) -> List String -> Decoder a -> Cmd msg
request method body toMsg path decode =
    Http.request
        { method = method
        , headers = []
        , url = String.join "/" path
        , body = body
        , expect = Http.expectJson toMsg decode
        , timeout = Nothing
        , tracker = Nothing
        }


requestGET : (Result Error a -> msg) -> List String -> Decoder a -> Cmd msg
requestGET =
    request "GET" Http.emptyBody


requestPOST : (Result Error a -> msg) -> List String -> Encode.Value -> Decoder a -> Cmd msg
requestPOST onMsg path body decode =
    request "POST" (Http.jsonBody body) onMsg path decode


requestPUT : (Result Error a -> msg) -> List String -> Encode.Value -> Decoder a -> Cmd msg
requestPUT onMsg path body decode =
    request "PUT" (Http.jsonBody body) onMsg path decode


requestDEL : (Result Error a -> msg) -> List String -> Decoder a -> Cmd msg
requestDEL =
    request "DELETE" Http.emptyBody


postApplications : (Result Error Application -> msg) -> AccountInfo -> Cmd msg
postApplications toMsg body =
    requestPOST toMsg [ "", "v1", "applications" ] (Application.encodeAccountInfo body) Application.decode


getAccount : (Result Error Account -> msg) -> Id AccountId -> Cmd msg
getAccount toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a ] decodeAccount


getAccountBanks : (Result Error (List BankAccount) -> msg) -> Id AccountId -> Cmd msg
getAccountBanks toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "bank-accounts" ] (list decodeBankAccount)


getCustomer : (Result Error Customer -> msg) -> Id AccountId -> Cmd msg
getCustomer toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "customer" ] decodeCustomer


getAccountHealth : (Result Error AccountHealth -> msg) -> Id AccountId -> Cmd msg
getAccountHealth toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "health" ] decodeAccountHealth


getApplication : (Result Error Application -> msg) -> Id AccountId -> Cmd msg
getApplication toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "application" ] Application.decode


getApplicationResult : (Result Error ApprovalResult -> msg) -> Id AccountId -> Cmd msg
getApplicationResult toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "application", "result" ] decodeApprovalResult


getAdvance : (Result Error Advance -> msg) -> Id AccountId -> Id AdvanceId -> Cmd msg
getAdvance toMsg (Id a) (Id adv) =
    requestGET toMsg [ "", "v1", "accounts", a, "advances", adv ] decodeAdvance


getAdvances : (Result Error (List Advance) -> msg) -> Id AccountId -> Cmd msg
getAdvances toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "advances" ] (list decodeAdvance)


getTransactions : (Result Error (List TransRow) -> msg) -> Id AccountId -> Cmd msg
getTransactions toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "transactions" ] (list decodeTransRow)


getTransactionHistory : (Result Error History -> msg) -> Id AccountId -> Cmd msg
getTransactionHistory toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "transactions", "history" ] decodeHistory


postAdvanceAccept : (Result Error Advance -> msg) -> Id AccountId -> Id AdvanceId -> Money -> Cmd msg
postAdvanceAccept toMsg (Id a) (Id adv) amt =
    requestPOST toMsg [ "", "v1", "accounts", a, "advances", adv, "accept" ] (encodeAmount { amount = amt }) decodeAdvance


getAvailableSubscriptions : (Result Error (List Subscription) -> msg) -> Cmd msg
getAvailableSubscriptions toMsg =
    requestGET toMsg [ "", "v1", "subscriptions" ] (list Subscription.decode)


getSubscription : (Result Error (Maybe Subscription) -> msg) -> Id AccountId -> Cmd msg
getSubscription toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "subscription" ] (nullable Subscription.decode)


putSubscription : (Result Error String -> msg) -> Id AccountId -> Subscription.Level -> Cmd msg
putSubscription toMsg (Id a) level =
    requestPUT toMsg [ "", "v1", "accounts", a, "subscription" ] (Subscription.encodeLevel level) string


delSubscription : (Result Error String -> msg) -> Id AccountId -> Cmd msg
delSubscription toMsg (Id a) =
    requestDEL toMsg [ "", "v1", "accounts", a, "subscription" ] string



-- Budgets ----------------------------


putBudget : BudgetType -> (Result Error String -> msg) -> Id AccountId -> Id BudgetId -> BudgetInfo -> Cmd msg
putBudget bt toMsg (Id ai) (Id bi) b =
    requestPUT toMsg [ "", "v1", "accounts", ai, budgetTypePath bt, bi ] (encodeBudget b) string


delBudget : BudgetType -> (Result Error String -> msg) -> Id AccountId -> Id BudgetId -> Cmd msg
delBudget bt toMsg (Id ai) (Id bi) =
    requestDEL toMsg [ "", "v1", "accounts", ai, budgetTypePath bt, bi ] string


getBudgets : BudgetType -> (Result Error (List Budget) -> msg) -> Id AccountId -> Cmd msg
getBudgets bt toMsg (Id ai) =
    requestGET toMsg [ "", "v1", "accounts", ai, budgetTypePath bt ] (list decodeBudget)


postBudget : BudgetType -> (Result Error (Id BudgetId) -> msg) -> Id AccountId -> BudgetInfo -> Cmd msg
postBudget bt toMsg (Id a) b =
    requestPOST toMsg [ "", "v1", "accounts", a, budgetTypePath bt ] (encodeBudget b) decodeId


putSpending : (Result Error String -> msg) -> Id AccountId -> Money -> Cmd msg
putSpending toMsg (Id ai) amount =
    requestPUT toMsg [ "", "v1", "accounts", ai, "spending" ] (encodeMoney amount) string


budgetTypePath : BudgetType -> String
budgetTypePath bt =
    case bt of
        Income ->
            "incomes"

        Expense ->
            "expenses"


editIncome =
    putBudget Income


editExpense =
    putBudget Expense


delIncome =
    delBudget Income


delExpense =
    delBudget Expense


getIncomes =
    getBudgets Income


getExpenses =
    getBudgets Expense


createIncome =
    postBudget Income


createExpense =
    postBudget Expense



-- Admin -------------------


getCustomers : (Result Error (List AccountCustomer) -> msg) -> Cmd msg
getCustomers toMsg =
    requestGET toMsg [ "", "v1", "admin", "customers" ] (list decodeAccountCustomer)


deleteAccount : (Result Error String -> msg) -> Id AccountId -> Cmd msg
deleteAccount toMsg (Id a) =
    requestDEL toMsg [ "", "v1", "admin", "accounts", a ] string



--- Authentication -------------------------------------------


sessionsCreateCode : (Result Error () -> msg) -> Phone -> Cmd msg
sessionsCreateCode toMsg p =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "sessions" ]
        , body = Http.jsonBody (encodeId p)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


sessionsCheckCode : (Result Error Session -> msg) -> Phone -> Token AuthCode -> Cmd msg
sessionsCheckCode toMsg (Id p) c =
    requestPOST toMsg [ "", "v1", "sessions", p ] (encodeId c) decodeSession


sessionsAuthAdmin : (Result Error Session -> msg) -> String -> Cmd msg
sessionsAuthAdmin toMsg s =
    requestPOST toMsg [ "", "v1", "sessions", "admin" ] (Encode.string s) decodeSession


sessionsGet : (Result Error Session -> msg) -> Cmd msg
sessionsGet toMsg =
    requestGET toMsg [ "", "v1", "sessions" ] decodeSession


sessionsLogout : (Result Error String -> msg) -> Cmd msg
sessionsLogout toMsg =
    requestDEL toMsg [ "", "v1", "sessions" ] string


expectId : (Result Error (Id a) -> msg) -> Expect msg
expectId toMsg =
    let
        onResult r =
            case r of
                Ok s ->
                    toMsg <| Ok (Id s)

                Err e ->
                    toMsg <| Err e
    in
    Http.expectString onResult


advanceIsOffer : Advance -> Bool
advanceIsOffer advance =
    case advance.activated of
        Nothing ->
            True

        Just _ ->
            False


advanceIsActive : Advance -> Bool
advanceIsActive advance =
    case ( advance.activated, advance.collected ) of
        ( Just _, Nothing ) ->
            True

        _ ->
            False


advanceIsCollected : Advance -> Bool
advanceIsCollected advance =
    case advance.collected of
        Just _ ->
            True

        _ ->
            False


usedCredit : List Advance -> Money
usedCredit advances =
    advances
        |> List.map (toCents << .amount)
        |> List.sum
        |> fromCents
