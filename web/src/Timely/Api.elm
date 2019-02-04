module Timely.Api exposing (Account, AccountInfo, Advance, Application, Approval, ApprovalResult(..), Auth(..), AuthCode(..), Balance, Bank(..), BankAccount, BankAccountType(..), Customer, Denial, Id(..), Money(..), Phone, Session, Token, decodeAccount, decodeAccountInfo, decodeApplication, decodeApproval, decodeApprovalResult, decodeBankAccount, decodeBankAccountType, decodeCustomer, decodeDenial, decodeId, decodeMoney, decodeSession, encodeAccountInfo, encodeId, expectId, getAccount, getAccountBanks, getAdvance, getApplicationResult, idValue, postApplications, sessionsCheckCode, sessionsCreateCode, sessionsGet, sessionsLogout)

import Http exposing (Error, Expect)
import Iso8601
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String
import Time


type Bank
    = Bank


type alias Account =
    { accountId : String
    , phone : String
    , customer : Customer
    }


type alias AccountInfo =
    { email : String
    , publicBankToken : Id Bank
    }


type alias Application =
    { accountId : Id Account
    }


type alias Session =
    { phone : Phone
    , accountId : Maybe (Id Account)
    }


type alias BankAccount =
    { accountId : String
    , accountType : BankAccountType
    , name : String
    , balance : Balance
    }


type BankAccountType
    = Checking
    | Savings
    | Credit
    | Other


type alias Balance =
    Int


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


type alias Advance =
    { advanceId : String
    , accountId : String
    , amount : Money
    , due : Time.Posix
    , offered : Time.Posix
    , activated : Maybe Time.Posix
    , collected : Maybe Time.Posix
    }


encodeAccountInfo : AccountInfo -> Encode.Value
encodeAccountInfo x =
    Encode.object
        [ ( "email", Encode.string x.email )
        , ( "publicBankToken", encodeId x.publicBankToken )
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
        |> required "accountId" string
        |> required "phone" string
        |> required "customer" decodeCustomer


decodeBankAccount : Decoder BankAccount
decodeBankAccount =
    Decode.succeed BankAccount
        |> required "accountId" string
        |> required "accountType" decodeBankAccountType
        |> required "name" string
        |> required "balance" int


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


decodeAdvance : Decoder Advance
decodeAdvance =
    Decode.succeed Advance
        |> required "advanceId" string
        |> required "accountId" string
        |> required "amount" decodeMoney
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


getAccount : (Result Error Account -> msg) -> Id Account -> Cmd msg
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


getAccountBanks : (Result Error (List BankAccount) -> msg) -> Id Account -> Cmd msg
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


getApplicationResult : (Result Error ApprovalResult -> msg) -> Id Account -> Cmd msg
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


getAdvance : (Result Error Advance -> msg) -> Id Account -> Id Advance -> Cmd msg
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
