module Timely.Api exposing (Account, AccountId, AccountInfo, Advance, AdvanceId, Amount, Application, Approval, ApprovalResult(..), Auth(..), AuthCode(..), Bank(..), BankAccount, BankAccountType(..), Customer, Denial, Id(..), Money, Onboarding(..), Phone, SSN, Session, Token, Transaction, Valid(..), advanceIsActive, advanceIsCollected, advanceIsOffer, decodeAccount, decodeAccountInfo, decodeAdvance, decodeApplication, decodeApproval, decodeApprovalResult, decodeBankAccount, decodeBankAccountType, decodeCustomer, decodeDenial, decodeId, decodeSession, encodeAccountInfo, encodeAmount, encodeId, expectId, formatDate, formatDollars, formatMoney, fromDollars, getAccount, getAccountBanks, getAdvance, getAdvances, getApplication, getApplicationResult, getTransactions, idValue, postAdvanceAccept, postApplications, sessionsAuthAdmin, sessionsCheckCode, sessionsCreateCode, sessionsGet, sessionsLogout, timezone, usedCredit)

import Http exposing (Error, Expect)
import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String
import Task
import Time exposing (Month(..))


type Bank
    = Bank


type AccountId
    = AccountId


type alias Account =
    { accountId : Id AccountId
    , phone : String
    , customer : Customer
    , credit : Money
    , health : AccountHealth
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


type alias Transaction =
    { transactionId : String
    , date : Time.Posix
    , category : String
    , pending : Bool
    , amount : Money
    , name : String
    }


type alias Customer =
    { accountId : String
    , firstName : String
    , middleName : Maybe String
    , lastName : String
    , email : String
    , ssn : Valid SSN
    , dateOfBirth : Time.Posix
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
    , due : Time.Posix
    , offered : Time.Posix
    , activated : Maybe Time.Posix
    , collected : Maybe Time.Posix
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
        |> required "customer" decodeCustomer
        |> required "credit" decodeMoney
        |> required "health" decodeHealth


decodeHealth : Decoder AccountHealth
decodeHealth =
    Decode.succeed AccountHealth
        |> required "expenses" decodeMoney
        |> required "available" decodeMoney



-- type alias Transaction =
--     { transactionId : String
--     , date : Time.Posix
--     , category : String
--     , pending : Bool
--     , amount : Money
--     , name : String
--     }


decodeTransaction : Decoder Transaction
decodeTransaction =
    Decode.succeed Transaction
        |> required "transactionId" string
        |> required "date" Iso8601.decoder
        |> required "category" string
        |> required "pending" bool
        |> required "amount" decodeMoney
        |> required "name" string


decodeBankAccount : Decoder BankAccount
decodeBankAccount =
    Decode.succeed BankAccount
        |> required "accountId" string
        |> required "accountType" decodeBankAccountType
        |> required "name" string
        |> required "balance" decodeMoney


decodeCustomer : Decoder Customer
decodeCustomer =
    Decode.succeed Customer
        |> required "accountId" string
        |> required "firstName" string
        |> required "middleName" (nullable string)
        |> required "lastName" string
        |> required "email" string
        |> required "ssn" decodeValid
        |> required "dateOfBirth" Iso8601.decoder


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


decodeMoney : Decoder Money
decodeMoney =
    int


encodeMoney : Money -> Encode.Value
encodeMoney =
    Encode.int


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
        |> required "due" Iso8601.decoder
        |> required "offered" Iso8601.decoder
        |> required "activated" (nullable Iso8601.decoder)
        |> required "collected" (nullable Iso8601.decoder)


postApplications : (Result Error Application -> msg) -> AccountInfo -> Cmd msg
postApplications toMsg body =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "applications" ]
        , body = Http.jsonBody (encodeAccountInfo body)
        , expect = Http.expectJson toMsg decodeApplication
        , timeout = Nothing
        , tracker = Nothing
        }


getAccount : (Result Error Account -> msg) -> Id AccountId -> Cmd msg
getAccount toMsg (Id a) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeAccount
        , timeout = Nothing
        , tracker = Nothing
        }


getAccountBanks : (Result Error (List BankAccount) -> msg) -> Id AccountId -> Cmd msg
getAccountBanks toMsg (Id a) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "bank-accounts" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (list decodeBankAccount)
        , timeout = Nothing
        , tracker = Nothing
        }


getApplication : (Result Error Application -> msg) -> Id AccountId -> Cmd msg
getApplication toMsg (Id a) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "application" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeApplication
        , timeout = Nothing
        , tracker = Nothing
        }


getApplicationResult : (Result Error ApprovalResult -> msg) -> Id AccountId -> Cmd msg
getApplicationResult toMsg (Id a) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "application", "result" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeApprovalResult
        , timeout = Nothing
        , tracker = Nothing
        }


getAdvance : (Result Error Advance -> msg) -> Id AccountId -> Id AdvanceId -> Cmd msg
getAdvance toMsg (Id a) (Id adv) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "advances", adv ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeAdvance
        , timeout = Nothing
        , tracker = Nothing
        }


getAdvances : (Result Error (List Advance) -> msg) -> Id AccountId -> Cmd msg
getAdvances toMsg (Id a) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "advances" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (list decodeAdvance)
        , timeout = Nothing
        , tracker = Nothing
        }


getTransactions : (Result Error (List Transaction) -> msg) -> Id AccountId -> Cmd msg
getTransactions toMsg (Id a) =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "transactions" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (list decodeTransaction)
        , timeout = Nothing
        , tracker = Nothing
        }


postAdvanceAccept : (Result Error Advance -> msg) -> Id AccountId -> Id AdvanceId -> Money -> Cmd msg
postAdvanceAccept toMsg (Id a) (Id adv) amt =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", a, "advances", adv, "accept" ]
        , body = Http.jsonBody (encodeAmount { amount = amt })
        , expect = Http.expectJson toMsg decodeAdvance
        , timeout = Nothing
        , tracker = Nothing
        }



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


type alias Money =
    Int



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
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "sessions", p ]
        , body = Http.jsonBody (encodeId c)
        , expect = Http.expectJson toMsg decodeSession
        , timeout = Nothing
        , tracker = Nothing
        }


sessionsAuthAdmin : (Result Error Session -> msg) -> String -> Cmd msg
sessionsAuthAdmin toMsg s =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "sessions", "admin" ]
        , body = Http.jsonBody (Encode.string s)
        , expect = Http.expectJson toMsg decodeSession
        , timeout = Nothing
        , tracker = Nothing
        }


sessionsGet : (Result Error Session -> msg) -> Cmd msg
sessionsGet toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "sessions" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeSession
        , timeout = Nothing
        , tracker = Nothing
        }


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


formatMoney : Money -> String
formatMoney i =
    let
        cents =
            String.padLeft 2 '0' <| String.fromInt (modBy 100 i)
    in
    formatDollars i ++ "." ++ cents


formatDollars : Money -> String
formatDollars i =
    String.fromInt (i // 100)


fromDollars : Int -> Money
fromDollars i =
    i * 100


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
        |> List.map .amount
        |> List.sum



-- Dates ----------------------------


formatDate : Time.Zone -> Time.Posix -> String
formatDate zone time =
    let
        formatYear t =
            String.fromInt <| Time.toYear zone time

        formatMonth t =
            case Time.toMonth zone t of
                Jan ->
                    "Jan"

                Feb ->
                    "Feb"

                Mar ->
                    "Mar"

                Apr ->
                    "Apr"

                May ->
                    "May"

                Jun ->
                    "Jun"

                Jul ->
                    "Jul"

                Aug ->
                    "Aug"

                Sep ->
                    "Sep"

                Oct ->
                    "Oct"

                Nov ->
                    "Nov"

                Dec ->
                    "Dec"

        formatDay t =
            String.fromInt <| Time.toDay zone time
    in
    formatMonth time ++ " " ++ formatDay time ++ ", " ++ formatYear time ++ " "


timezone : (Time.Zone -> msg) -> Cmd msg
timezone toMsg =
    Task.perform toMsg Time.here
