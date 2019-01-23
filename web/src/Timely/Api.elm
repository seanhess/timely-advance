module Timely.Api exposing (Account, AccountId, AccountInfo, Application, Approval, ApprovalResult(..), Auth(..), AuthCode(..), Balance, Bank(..), BankAccount, BankAccountType(..), Customer, Denial, Id(..), Phone, Session, Token, decodeAccount, decodeAccountInfo, decodeApplication, decodeApproval, decodeApprovalResult, decodeBankAccount, decodeBankAccountType, decodeCustomer, decodeDenial, decodeId, encodeAccountInfo, encodeId, expectId, getAccount, getAccountBanks, getApplicationResult, getTest, idValue, postApplications, postLogin, postLogout, sessionsCheckCode, sessionsCreateCode)

import Http exposing (Error, Expect)
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String


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


type alias AccountId =
    String


type alias Application =
    { accountId : AccountId
    }


type alias Session =
    { phone : Phone
    , accountId : Maybe AccountId
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
        |> required "accountId" string


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



-- getAccounts : (Result Error (List Account) -> msg) -> Cmd msg
-- getAccounts toMsg =
--     Http.request
--         { method = "GET"
--         , headers = []
--         , url = String.join "/" [ "", "v1", "accounts" ]
--         , body = Http.emptyBody
--         , expect = Http.expectJson toMsg (list decodeAccount)
--         , timeout = Nothing
--         , tracker = Nothing
--         }


postApplications : (Result Error Application -> msg) -> AccountInfo -> Cmd msg
postApplications toMsg body =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "application" ]
        , body = Http.jsonBody (encodeAccountInfo body)
        , expect = Http.expectJson toMsg decodeApplication
        , timeout = Nothing
        , tracker = Nothing
        }


getAccount : (Result Error Account -> msg) -> Cmd msg
getAccount toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "account" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeAccount
        , timeout = Nothing
        , tracker = Nothing
        }


getAccountBanks : (Result Error (List BankAccount) -> msg) -> Cmd msg
getAccountBanks toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "account", "bank-accounts" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (list decodeBankAccount)
        , timeout = Nothing
        , tracker = Nothing
        }


getApplicationResult : (Result Error ApprovalResult -> msg) -> Cmd msg
getApplicationResult toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "application", "result" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeApprovalResult
        , timeout = Nothing
        , tracker = Nothing
        }


postLogin : (Result Error () -> msg) -> Cmd msg
postLogin toMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "login" ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


postLogout : (Result Error () -> msg) -> Cmd msg
postLogout toMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "logout" ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


getTest : (Result Error String -> msg) -> Cmd msg
getTest toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "test" ]
        , body = Http.emptyBody
        , expect = Http.expectString toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- Authentication -----------------


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


sessionsCheckCode : (Result Error () -> msg) -> Phone -> Token AuthCode -> Cmd msg
sessionsCheckCode toMsg (Id p) c =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "v1", "sessions", p ]
        , body = Http.jsonBody (encodeId c)
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
