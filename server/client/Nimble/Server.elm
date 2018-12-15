module Nimble.Server exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Account =
    { accountId : String
    , accountInfo : AccountInfo
    }

type alias AccountInfo =
    { firstName : String
    , lastName : String
    , email : String
    }

decodeAccount : Decoder Account
decodeAccount =
    decode Account
        |> required "accountId" string
        |> required "accountInfo" decodeAccountInfo

encodeAccountInfo : AccountInfo -> Json.Encode.Value
encodeAccountInfo x =
    Json.Encode.object
        [ ( "firstName", Json.Encode.string x.firstName )
        , ( "lastName", Json.Encode.string x.lastName )
        , ( "email", Json.Encode.string x.email )
        ]

decodeAccountInfo : Decoder AccountInfo
decodeAccountInfo =
    decode AccountInfo
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string

get : Http.Request (String)
get =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAccounts : Http.Request (List (Account))
getAccounts =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "accounts"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeAccount)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAccounts : AccountInfo -> Http.Request (Account)
postAccounts body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "accounts"
                ]
        , body =
            Http.jsonBody (encodeAccountInfo body)
        , expect =
            Http.expectJson decodeAccount
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAccountsById : String -> Http.Request (Account)
getAccountsById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "accounts"
                , capture_id |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeAccount
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putAccountsById : String -> AccountInfo -> Http.Request (Account)
putAccountsById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "accounts"
                , capture_id |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeAccountInfo body)
        , expect =
            Http.expectJson decodeAccount
        , timeout =
            Nothing
        , withCredentials =
            False
        }