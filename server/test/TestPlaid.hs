{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestPlaid where

import Data.String.Conversions
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Plaid
import Network.Plaid.Types
import Servant.Client
import Text.Show.Pretty

main :: IO ()
main = putStrLn "Test suite not yet implemented"

deriving instance PrettyVal AuthResponse
deriving instance PrettyVal AccountsResponse
deriving instance PrettyVal (Id a)
deriving instance PrettyVal Account
deriving instance PrettyVal Balances
deriving instance PrettyVal Currency
deriving instance PrettyVal CurrencyCode
deriving instance PrettyVal AccountType
deriving instance PrettyVal AccountSubType
deriving instance PrettyVal AuthNumbers
deriving instance PrettyVal Ach
deriving instance PrettyVal Eft
deriving instance PrettyVal (Number a)
instance PrettyVal Item where
  prettyVal _ = String "{item}"


test :: IO ()
test = do
    -- putStrLn "Enter public token"
    -- tok <- getLine
    -- putStrLn $ "Using token: " ++ tok

    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https "sandbox.plaid.com" 443 "")
    let access = Access (Id "5c1a663c5eca930011ff67ee") (Id "db8bad5d68d41340cba767615c7aea") (Token "access-sandbox-42065f86-d64a-4a9d-abd2-b1226aaf693f")
    -- print =<< runClientM (sendExchangeToken $ ExchangeTokenRequest (Id "5c1a663c5eca930011ff67ee") (Id "db8bad5d68d41340cba767615c7aea") (Token (cs tok))) env

    -- res <- runClientM (sendAuth $ AuthRequest (Id "5c1a663c5eca930011ff67ee") (Id "db8bad5d68d41340cba767615c7aea") (Token "access-sandbox-42065f86-d64a-4a9d-abd2-b1226aaf693f")) env


    res <- runClientM (sendAccounts $ accountsRequest access) env
    case res of
      Left (DecodeFailure t r) -> do
        putStrLn $ cs $ t
        putStrLn $ cs $ responseBody r
      Right b -> do
        putStrLn $ dumpStr b
        -- putStrLn $  b

    pure ()

-- it worked!
-- Right (ExchangeTokenResponse {access_token = Token "access-sandbox-42065f86-d64a-4a9d-abd2-b1226aaf693f", item_id = Id "qxBMPMEWNet9JLDW3GJoUA7nnaZkMziddn1No", request_id = Id "Mfl5nPKQnl6YU1S"})
-- "access-sandbox-42065f86-d64a-4a9d-abd2-b1226aaf693f"



-- Left (DecodeFailure "Error in $.accounts[0].balances: expected [a], encountered Object" (Response {responseStatusCode = Status {statusCode = 200, statusMessage = "OK"}, responseHeaders = fromList [("Server","nginx"),("Date","Fri, 21 Dec 2018 18:23:57 GMT"),("Content-Type","application/json"),("Content-Length","834"),("Connection","keep-alive"),("Content-Encoding","gzip"),("Plaid-Version","2018-05-22"),("Vary","Accept-Encoding"),("Strict-Transport-Security","max-age=31536000; includeSubDomains; preload"),("X-Frame-Options","DENY"),("X-Content-Type-Options","nosniff"),("X-XSS-Protection","1; mode=block")], responseHttpVersion = HTTP/1.1, responseBody = "{\n  \"accounts\": [\n    {\n      \"account_id\": \"x7Py1y39Eof6J9nXBgJzCVpq7bWanxcnNgxaZ\",\n      \"balances\": {\n        \"available\": 100,\n        \"current\": 110,\n        \"iso_currency_code\": \"USD\",\n        \"limit\": null,\n        \"unofficial_currency_code\": null\n      },\n      \"mask\": \"0000\",\n      \"name\": \"Plaid Checking\",\n      \"official_name\": \"Plaid Gold Standard 0% Interest Checking\",\n      \"subtype\": \"checking\",\n      \"type\": \"depository\"\n    },\n    {\n      \"account_id\": \"4oKDNDQBM4SLZBe5ygZoc1yGbDWNEaUdkWXyg\",\n      \"balances\": {\n        \"available\": 200,\n        \"current\": 210,\n        \"iso_currency_code\": \"USD\",\n        \"limit\": null,\n        \"unofficial_currency_code\": null\n      },\n      \"mask\": \"1111\",\n      \"name\": \"Plaid Saving\",\n      \"official_name\": \"Plaid Silver Standard 0.1% Interest Saving\",\n      \"subtype\": \"savings\",\n      \"type\": \"depository\"\n    },\n    {\n      \"account_id\": \"a5MKvKa4oAhnWBPKRkWdInamAZJXdyu7Pn8mp\",\n      \"balances\": {\n        \"available\": null,\n        \"current\": 1000,\n        \"iso_currency_code\": \"USD\",\n        \"limit\": null,\n        \"unofficial_currency_code\": null\n      },\n      \"mask\": \"2222\",\n      \"name\": \"Plaid CD\",\n      \"official_name\": \"Plaid Bronze Standard 0.2% Interest CD\",\n      \"subtype\": \"cd\",\n      \"type\": \"depository\"\n    },\n    {\n      \"account_id\": \"d5pBvBPjGKhynmNk7MnlUnp1lEwVGxuZG9v1d\",\n      \"balances\": {\n        \"available\": null,\n        \"current\": 410,\n        \"iso_currency_code\": \"USD\",\n        \"limit\": 2000,\n        \"unofficial_currency_code\": null\n      },\n      \"mask\": \"3333\",\n      \"name\": \"Plaid Credit Card\",\n      \"official_name\": \"Plaid Diamond 12.5% APR Interest Credit Card\",\n      \"subtype\": \"credit card\",\n      \"type\": \"credit\"\n    },\n    {\n      \"account_id\": \"N5G6v6QmAdh4xvGw83xzSKgwMQ5Ak9SWxaEJW\",\n      \"balances\": {\n        \"available\": 43200,\n        \"current\": 43200,\n        \"iso_currency_code\": \"USD\",\n        \"limit\": null,\n        \"unofficial_currency_code\": null\n      },\n      \"mask\": \"4444\",\n      \"name\": \"Plaid Money Market\",\n      \"official_name\": \"Plaid Platinum Standard 1.85% Interest Money Market\",\n      \"subtype\": \"money market\",\n      \"type\": \"depository\"\n    }\n  ],\n  \"item\": {\n    \"available_products\": [\n      \"assets\",\n      \"balance\",\n      \"credit_details\",\n      \"identity\",\n      \"income\"\n    ],\n    \"billed_products\": [\n      \"auth\",\n      \"transactions\"\n    ],\n    \"error\": null,\n    \"institution_id\": \"ins_3\",\n    \"item_id\": \"qxBMPMEWNet9JLDW3GJoUA7nnaZkMziddn1No\",\n    \"webhook\": \"\"\n  },\n  \"numbers\": {\n    \"ach\": [\n      {\n        \"account\": \"1111222233330000\",\n        \"account_id\": \"x7Py1y39Eof6J9nXBgJzCVpq7bWanxcnNgxaZ\",\n        \"routing\": \"011401533\",\n        \"wire_routing\": \"021000021\"\n      },\n      {\n        \"account\": \"1111222233331111\",\n        \"account_id\": \"4oKDNDQBM4SLZBe5ygZoc1yGbDWNEaUdkWXyg\",\n        \"routing\": \"011401533\",\n        \"wire_routing\": \"021000021\"\n      }\n    ],\n    \"eft\": []\n  },\n  \"request_id\": \"Rej1Zqcr3T7ehzi\"\n}"}))
