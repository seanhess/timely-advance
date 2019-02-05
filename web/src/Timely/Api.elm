module Timely.Api exposing (Account, AccountId, AccountInfo, Advance, AdvanceId, Amount, Application, Approval, ApprovalResult(..), Auth(..), AuthCode(..), Bank(..), BankAccount, BankAccountType(..), Customer, Denial, Id(..), Money(..), Phone, Session, Token, decodeAccount, decodeAccountInfo, decodeAdvance, decodeApplication, decodeApproval, decodeApprovalResult, decodeBankAccount, decodeBankAccountType, decodeCustomer, decodeDenial, decodeId, decodeMoney, decodeSession, encodeAccountInfo, encodeAmount, encodeId, encodeMoney, expectId, formatDollars, formatMoney, fromDollars, getAccount, getAccountBanks, getAdvance, getAdvances, getApplicationResult, idValue, postAdvanceAccept, postApplications, sessionsCheckCode, sessionsCreateCode, sessionsGet, sessionsLogout)

import Http exposing (Error, Expect)
import Iso8601
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String
import Time


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
    , publicBankToken : Id Bank
    }


type alias Application =
    { accountId : Id AccountId
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


type alias Customer =
    { accountId : String
    , firstName : String
    , middleName : Maybe String
    , lastName : String
    , email : String
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


decodeApplication : Decoder Application
decodeApplication =
    Decode.succeed Application
        |> required "accountId" decodeId


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


decodeMoney : Decoder Money
decodeMoney =
    Decode.map Money
        int


encodeMoney : Money -> Encode.Value
encodeMoney (Money m) =
    Encode.int m


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


type Money
    = Money Int



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
formatMoney (Money i) =
    let
        cents =
            String.padLeft 2 '0' <| String.fromInt (modBy 100 i)
    in
    formatDollars (Money i) ++ "." ++ cents


formatDollars : Money -> String
formatDollars (Money i) =
    String.fromInt (i // 100)


fromDollars : Int -> Money
fromDollars i =
    Money (i * 100)
