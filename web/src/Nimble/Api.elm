module Nimble.Api exposing (Account, AccountInfo, decodeAccount, decodeAccountInfo, encodeAccountInfo, getAccounts, getAccountsById, postAccounts, putAccountsById)

import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Account =
    { accountId : String
    , firstName : String
    , lastName : String
    , email : String
    }


type alias AccountInfo =
    { firstName : String
    , lastName : String
    , email : String
    }


decodeAccount : Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "accountId" string
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string


encodeAccountInfo : AccountInfo -> Json.Encode.Value
encodeAccountInfo x =
    Json.Encode.object
        [ ( "firstName", Json.Encode.string x.firstName )
        , ( "lastName", Json.Encode.string x.lastName )
        , ( "email", Json.Encode.string x.email )
        ]


decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    Decode.succeed AccountInfo
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string


getAccounts : (Result Error (List Account) -> msg) -> Cmd msg
getAccounts toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "accounts" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (list decodeAccount)
        , timeout = Nothing
        , tracker = Nothing
        }


postAccounts : (Result Error Account -> msg) -> AccountInfo -> Cmd msg
postAccounts toMsg body =
    Http.request
        { method = "POST"
        , headers = []
        , url = String.join "/" [ "", "accounts" ]
        , body = Http.jsonBody (encodeAccountInfo body)
        , expect = Http.expectJson toMsg decodeAccount
        , timeout = Nothing
        , tracker = Nothing
        }


getAccountsById : (Result Error Account -> msg) -> String -> Cmd msg
getAccountsById toMsg id =
    Http.request
        { method = "GET"
        , headers = []
        , url = String.join "/" [ "", "accounts", id ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decodeAccount
        , timeout = Nothing
        , tracker = Nothing
        }


putAccountsById : (Result Error Account -> msg) -> String -> AccountInfo -> Cmd msg
putAccountsById toMsg id body =
    Http.request
        { method = "PUT"
        , headers = []
        , url = String.join "/" [ "", "accounts", id ]
        , body = Http.jsonBody (encodeAccountInfo body)
        , expect = Http.expectJson toMsg decodeAccount
        , timeout = Nothing
        , tracker = Nothing
        }
