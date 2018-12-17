module Nimble.Api exposing (Account, AccountInfo, decodeAccount, decodeAccountInfo, encodeAccountInfo, get, getAccounts, getAccountsById, postAccounts, putAccountsById)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String.Conversions as String
import Url


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
    succeed Account
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
    succeed AccountInfo
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string


get : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) String -> msg) -> Cmd msg
get toMsg =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString string body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getAccounts : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) (List Account) -> msg) -> Cmd msg
getAccounts toMsg =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString (list decodeAccount) body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postAccounts : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Account -> msg) -> AccountInfo -> Cmd msg
postAccounts toMsg body =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString decodeAccount body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getAccountsById : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Account -> msg) -> String -> Cmd msg
getAccountsById toMsg capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "accounts"
                , capture_id |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString decodeAccount body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


putAccountsById : (Result ( Maybe ( Http.Metadata, String ), Http.Error ) Account -> msg) -> String -> AccountInfo -> Cmd msg
putAccountsById toMsg capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "accounts"
                , capture_id |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeAccountInfo body)
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url ->
                            Err ( Nothing, Http.BadUrl url )

                        Http.Timeout_ ->
                            Err ( Nothing, Http.Timeout )

                        Http.NetworkError_ ->
                            Err ( Nothing, Http.NetworkError )

                        Http.BadStatus_ metadata body_ ->
                            Err ( Just ( metadata, body_ ), Http.BadStatus metadata.statusCode )

                        Http.GoodStatus_ metadata body_ ->
                            decodeString decodeAccount body_
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just ( metadata, body_ )))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
