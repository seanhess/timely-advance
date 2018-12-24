module Nimble.Api exposing (Account, AccountInfo, Application, Bank, Customer, decodeAccount, encodeAccountInfo, getAccountsById, postApplications)

import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Account =
    { accountId : String
    , bank : Bank
    , customer : Customer
    }


type alias AccountInfo =
    { firstName : String
    , lastName : String
    , email : String
    , plaidToken : String
    }


type alias Application =
    { accountId : String
    , firstName : String
    , lastName : String
    , email : String
    , plaidToken : String
    }


type alias Bank =
    { accountId : String
    , balance : Int
    , accessToken : String
    }


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
        , ( "plaidToken", Json.Encode.string x.plaidToken )
        ]


decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    Decode.succeed AccountInfo
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string
        |> required "plaidToken" string


decodeAccount : Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "accountId" string
        |> required "bank" decodeBank
        |> required "customer" decodeCustomer


decodeBank : Decoder Bank
decodeBank =
    Decode.succeed Bank
        |> required "accountId" string
        |> required "balance" int
        |> required "accessToken" string


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
        |> required "plaidToken" string



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
