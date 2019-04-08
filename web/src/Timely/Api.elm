module Timely.Api exposing (Account, AccountCustomer, AccountHealth, AccountId(..), AccountInfo, Advance, AdvanceId(..), Amount, Application, Approval, ApprovalResult(..), Auth(..), AuthCode(..), Bank(..), BankAccount, BankAccountType(..), Customer, Denial, Id(..), Onboarding(..), Phone, SSN(..), Session, Token, Valid(..), advanceIsActive, advanceIsCollected, advanceIsOffer, decodeAccount, decodeAccountInfo, decodeAdvance, decodeApplication, decodeApproval, decodeApprovalResult, decodeBankAccount, decodeBankAccountType, decodeCustomer, decodeDenial, decodeHealth, decodeId, decodeOnboarding, decodeSession, decodeValid, encodeAccountInfo, encodeAmount, encodeId, encodeValid, expectId, getAccount, getAccountBanks, getAccountHealth, getAdvance, getAdvances, getApplication, getApplicationResult, getCustomer, getCustomers, getTransactionHistory, getTransactions, idValue, postAdvanceAccept, postApplications, request, requestGET, requestPOST, sessionsAuthAdmin, sessionsCheckCode, sessionsCreateCode, sessionsGet, sessionsLogout, usedCredit)

import Http exposing (Error, Expect)
import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String
import Task
import Time exposing (Month(..))
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney, fromCents, toCents)
import Timely.Types.Transactions exposing (History, Transaction, decodeHistory, decodeTransaction)


type Bank
    = Bank


type AccountId
    = AccountId


type alias Account =
    { accountId : Id AccountId
    , phone : String
    , credit : Money
    }


type alias AccountHealth =
    { expenses : Money
    , available : Money
    }


type alias AccountInfo =
    { email : String
    , ssn : Valid SSN
    , dateOfBirth : String
    , publicBankToken : Id Bank
    }


type alias Application =
    { accountId : Id AccountId
    , onboarding : Onboarding
    }


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
    { accountId : String
    , firstName : String
    , middleName : Maybe String
    , lastName : String
    , email : String
    , ssn : Valid SSN
    , dateOfBirth : Date
    }


type alias AccountCustomer =
    { account : Account
    , customer : Customer
    }


type SSN
    = SSN


type Valid a
    = Valid String


type ApprovalResult
    = Approved Approval
    | Denied Denial


type Onboarding
    = Pending
    | Complete
    | Error


type alias Approval =
    { approvalAmount : Int
    }


type alias Denial =
    { denial : String
    }


type AdvanceId
    = AdvanceId


type alias Advance =
    { advanceId : Id AdvanceId
    , accountId : Id AccountId
    , amount : Money
    , offer : Money
    , due : Date
    , offered : Date
    , activated : Maybe Date
    , collected : Maybe Date
    }


type alias Amount =
    { amount : Money
    }


encodeAccountInfo : AccountInfo -> Encode.Value
encodeAccountInfo x =
    Encode.object
        [ ( "email", Encode.string x.email )
        , ( "ssn", encodeValid x.ssn )
        , ( "dateOfBirth", Encode.string x.dateOfBirth )
        , ( "publicBankToken", encodeId x.publicBankToken )
        ]


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


decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    Decode.succeed AccountInfo
        |> required "email" string
        |> required "ssn" decodeValid
        |> required "dateOfBirth" string
        |> required "publicBankToken" decodeId


decodeAccount : Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "accountId" decodeId
        |> required "phone" string
        |> required "credit" decodeMoney


decodeHealth : Decoder AccountHealth
decodeHealth =
    Decode.succeed AccountHealth
        |> required "expenses" decodeMoney
        |> required "available" decodeMoney



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
        |> required "accountId" string
        |> required "firstName" string
        |> required "middleName" (nullable string)
        |> required "lastName" string
        |> required "email" string
        |> required "ssn" decodeValid
        |> required "dateOfBirth" decodeDate


decodeApplication : Decoder Application
decodeApplication =
    Decode.succeed Application
        |> required "accountId" decodeId
        |> required "onboarding" decodeOnboarding


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


decodeOnboarding : Decoder Onboarding
decodeOnboarding =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Pending" ->
                        Decode.succeed Pending

                    "Error" ->
                        Decode.succeed Error

                    "Complete" ->
                        Decode.succeed Complete

                    _ ->
                        Decode.fail ("Invalid Onboarding, " ++ string)
            )


decodeValid : Decoder (Valid a)
decodeValid =
    Decode.map Valid string


encodeValid : Valid a -> Encode.Value
encodeValid (Valid t) =
    Encode.string t


decodeAdvance : Decoder Advance
decodeAdvance =
    Decode.succeed Advance
        |> required "advanceId" decodeId
        |> required "accountId" decodeId
        |> required "amount" decodeMoney
        |> required "offer" decodeMoney
        |> required "due" decodeDate
        |> required "offered" decodeDate
        |> required "activated" (nullable decodeDate)
        |> required "collected" (nullable decodeDate)


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


postApplications : (Result Error Application -> msg) -> AccountInfo -> Cmd msg
postApplications toMsg body =
    requestPOST toMsg [ "", "v1", "applications" ] (encodeAccountInfo body) decodeApplication


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
    requestGET toMsg [ "", "v1", "accounts", a, "health" ] decodeHealth


getApplication : (Result Error Application -> msg) -> Id AccountId -> Cmd msg
getApplication toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "application" ] decodeApplication


getApplicationResult : (Result Error ApprovalResult -> msg) -> Id AccountId -> Cmd msg
getApplicationResult toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "application", "result" ] decodeApprovalResult


getAdvance : (Result Error Advance -> msg) -> Id AccountId -> Id AdvanceId -> Cmd msg
getAdvance toMsg (Id a) (Id adv) =
    requestGET toMsg [ "", "v1", "accounts", a, "advances", adv ] decodeAdvance


getAdvances : (Result Error (List Advance) -> msg) -> Id AccountId -> Cmd msg
getAdvances toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "advances" ] (list decodeAdvance)


getTransactions : (Result Error (List Transaction) -> msg) -> Id AccountId -> Cmd msg
getTransactions toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "transactions" ] (list decodeTransaction)


getTransactionHistory : (Result Error History -> msg) -> Id AccountId -> Cmd msg
getTransactionHistory toMsg (Id a) =
    requestGET toMsg [ "", "v1", "accounts", a, "transactions", "history" ] decodeHistory


postAdvanceAccept : (Result Error Advance -> msg) -> Id AccountId -> Id AdvanceId -> Money -> Cmd msg
postAdvanceAccept toMsg (Id a) (Id adv) amt =
    requestPOST toMsg [ "", "v1", "accounts", a, "advances", adv, "accept" ] (encodeAmount { amount = amt }) decodeAdvance



-- Admin -------------------


getCustomers : (Result Error (List AccountCustomer) -> msg) -> Cmd msg
getCustomers toMsg =
    requestGET toMsg [ "", "v1", "admin", "customers" ] (list decodeAccountCustomer)



-- Common -----------------


type alias Phone =
    Id ()


type AuthCode
    = AuthCode


type Auth
    = Auth


type Id a
    = Id String


type alias Token a =
    Id a


idValue : Id a -> String
idValue (Id a) =
    a


encodeId : Id a -> Encode.Value
encodeId (Id s) =
    Encode.string s


decodeId : Decoder (Id a)
decodeId =
    Decode.map Id Decode.string



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


sessionsLogout : (Result Error () -> msg) -> Cmd msg
sessionsLogout toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = String.join "/" [ "", "v1", "sessions" ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


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
