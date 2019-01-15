module Timely.Api exposing (Account, AccountId, AccountInfo, Application, Approval, ApprovalResult(..), Balance, BankAccount, BankAccountType(..), Customer, Denial, Token, decodeAccount, decodeAccountInfo, decodeApplication, decodeApproval, decodeApprovalResult, decodeBankAccount, decodeBankAccountType, decodeCustomer, decodeDenial, encodeAccountInfo, getAccountsBanksById, getAccountsById, getApplicationResultById, postApplications)

import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Token =
    String


type alias Account =
    { accountId : String
    , customer : Customer
    }


type alias AccountInfo =
    { phone : String
    , email : String
    , publicBankToken : Token
    }


type alias AccountId =
    String


type alias Application =
    { accountId : AccountId
    , firstName : String
    , lastName : String
    , email : String
    , publicBankToken : String
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


encodeAccountInfo : AccountInfo -> Json.Encode.Value
encodeAccountInfo x =
    Json.Encode.object
        [ ( "phone", Json.Encode.string x.phone )
        , ( "email", Json.Encode.string x.email )
        , ( "publicBankToken", Json.Encode.string x.publicBankToken )
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
        |> required "phone" string
        |> required "email" string
        |> required "publicBankToken" string


decodeAccount : Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "accountId" string
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
        |> required "lastName" string
        |> required "email" string


decodeApplication : Decoder Application
decodeApplication =
    Decode.succeed Application
        |> required "accountId" string
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string
        |> required "publicBankToken" string


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
        , url = String.join "/" [ "", "v1", "applications" ]
        , body = Http.jsonBody (encodeAccountInfo body)
        , expect = Http.expectJson toMsg decodeApplication
        , timeout = Nothing
        , tracker = Nothing
        }


getAccountsById : (Result Error Account -> msg) -> String -> Cmd msg
getAccountsById toMsg id =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", id ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeAccount
        , timeout = Nothing
        , tracker = Nothing
        }


getAccountsBanksById : (Result Error (List BankAccount) -> msg) -> String -> Cmd msg
getAccountsBanksById toMsg id =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "accounts", id, "bank-accounts" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (list decodeBankAccount)
        , timeout = Nothing
        , tracker = Nothing
        }


getApplicationResultById : (Result Error ApprovalResult -> msg) -> AccountId -> Cmd msg
getApplicationResultById toMsg id =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "v1", "applications", id, "result" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeApprovalResult
        , timeout = Nothing
        , tracker = Nothing
        }



-- putAccountsById : (Result Error Account -> msg) -> String -> AccountInfo -> Cmd msg
-- putAccountsById toMsg id body =
--     Http.request
--         { method = "PUT"
--         , headers = []
--         , url = String.join "/" [ "", "v1", "accounts", id ]
--         , body = Http.jsonBody (encodeAccountInfo body)
--         , expect = Http.expectJson toMsg decodeAccount
--         , timeout = Nothing
--         , tracker = Nothing
--         }
