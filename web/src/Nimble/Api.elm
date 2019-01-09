module Nimble.Api exposing (Account, AccountInfo, Application, Balance, BankAccount, BankAccountType(..), Customer, Token, decodeAccount, decodeAccountInfo, decodeApplication, decodeBankAccount, decodeCustomer, encodeAccountInfo, getAccountsBanksById, getAccountsById, postApplications)

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
    { firstName : String
    , lastName : String
    , email : String
    , publicBankToken : Token
    }


type alias Application =
    { accountId : String
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


encodeAccountInfo : AccountInfo -> Json.Encode.Value
encodeAccountInfo x =
    Json.Encode.object
        [ ( "firstName", Json.Encode.string x.firstName )
        , ( "lastName", Json.Encode.string x.lastName )
        , ( "email", Json.Encode.string x.email )
        , ( "publicBankToken", Json.Encode.string x.publicBankToken )
        ]


decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    Decode.succeed AccountInfo
        |> required "firstName" string
        |> required "lastName" string
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
