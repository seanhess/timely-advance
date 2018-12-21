{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Plaid where


import Data.Aeson (Value, ToJSON, FromJSON, parseJSON, withText, genericToJSON, genericParseJSON, defaultOptions, Options(..))
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)


-- ignore snake case errors
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}



-- GENERAL

-- Token types
data Public
data Access

-- Id Types
data Client
data Secret
data Request


-- Number
data Routing
data WireRouting
data Institution
data Branch


newtype Token t = Token Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq)

newtype Id t = Id Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq)

newtype Number t = Number Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq)

newtype Item = Item Value
    deriving (Generic, FromJSON, ToJSON, Show, Eq)


newtype Currency = Currency Float
    deriving (Generic, FromJSON, ToJSON, Show, Eq)

newtype CurrencyCode = CurrencyCode Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq)




-- EXCHANGE TOKEN :) Public Tokens are short lived, and must be exchanged for an access token

data TokenExchangeRequest = TokenExchangeRequest
    { client_id :: Id Client
    , secret :: Id Secret
    , public_token :: Token Public
    } deriving (Generic, Show, Eq)

instance ToJSON TokenExchangeRequest

data TokenExchangeResponse = TokenExchangeResponse
    { access_token :: Token Access
    , item_id :: Id Item
    , request_id :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON TokenExchangeResponse



-- curl -X POST https://sandbox.plaid.com/item/public_token/exchange \
--   -H 'Content-Type: application/json' \
--   -d '{
--     "client_id": String,
--     "secret": String,
--     "public_token": 
-- "public-sandbox-5c224a01-8314-4491-a06f-39e193d5cddc"
--   }'

-- http code 200
-- {
--   "access_token": 
-- "access-sandbox-de3ce8ef-33f8-452c-a685-8671031fc0f6",
--   "item_id": "M5eVJqLnv3tbzdngLDp9FL5OlDNxlNhlE55op",
--   "request_id": "Aim3b"
-- }


-- GET BANK ACCOUNT INFORMATION (ACH)


enumOptions :: Options
enumOptions = defaultOptions { constructorTagModifier = map toLower }

data AccountType
    = Depository
    | Credit
    | Brokerage
    | Loan
    | Other
    deriving (Generic, Eq, Show)

instance FromJSON AccountType where
    parseJSON = genericParseJSON enumOptions

-- there are many sub types, and they are specific to the account type
-- add more we care about later
data AccountSubType
    = Checking
    | Savings
    | SubType Text
    deriving (Generic, Eq, Show)

instance FromJSON AccountSubType where
  parseJSON = withText "AccountSubType" (pure . subType)
    where
      subType "checking" = Checking
      subType "savings" = Savings
      subType t = SubType t

data Account = Account
    { accountId :: Id Account
    , balances :: [ Balance ]
    , mask :: Text
    , name :: Text
    , officialName :: Text
    , subtype :: AccountSubType
    , _type :: AccountType
    } deriving (Generic, Show, Eq)

instance FromJSON Account where
    parseJSON = genericParseJSON defaultOptions
                  { fieldLabelModifier = dropWhile (== '_') }

data Balance = Balance
    { current :: Currency
    , available :: Maybe Currency
    , isoCurrencyCode :: Maybe CurrencyCode
    , unofficialCurrencyCode :: Maybe CurrencyCode
    } deriving (Generic, Show, Eq)

instance FromJSON Balance


data AuthRequest = AuthRequest
    { clientId :: Id Client
    , secret :: Id Secret
    , accessToken :: Token Access
    } deriving (Generic, Show, Eq)

instance FromJSON AuthRequest


data AuthResponse = AuthResponse
    { accounts :: [ Account ]
    , numbers :: AuthNumbers
    , requestId :: Id Request
    , item :: Item
    } deriving (Generic, Show, Eq)

instance FromJSON AuthResponse

data AuthNumbers = AuthNumbers
    { ach :: [ Ach ]
    , eft :: [ Eft ]
    } deriving (Generic, Show, Eq)

instance FromJSON AuthNumbers


data Ach = Ach
    { accountId :: Id Account
    , account :: Number Account
    , routing :: Number Routing
    , wireRouting :: Maybe (Number WireRouting)
    } deriving (Generic, Show, Eq)

instance FromJSON Ach

data Eft = Eft
    { accountId :: Id Account
    , account :: Number Account
    , institution :: Number Institution
    , branch :: Number Branch
    } deriving (Generic, Show, Eq)

instance FromJSON Eft

-- curl -X POST https://sandbox.plaid.com/auth/get \
-- -H 'Content-Type: application/json' \
-- -d '{
--   "client_id": String,
--   "secret": String,
--   "access_token": String
-- }'

-- http code 200
-- {
--   "accounts": [{
--     "account_id": "vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D",
--     "balances": {
--       "available": 100,
--       "current": 110,
--       "limit": null,
--       "iso_currency_code": "USD",
--       "unofficial_currency_code": null,
--     },
--     "mask": "0000",
--     "name": "Plaid Checking",
--     "official_name": "Plaid Gold Checking",
--     "subtype": "checking",
--     "type": "depository"
--   }],
--   "numbers": {
--      "ach": [{
--       "account": "9900009606",
--       "account_id": "vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D",
--       "routing": "011401533",
--       "wire_routing": "021000021"
--      }],
--      "eft": []
--   },
--   "item": {Object},
--   "request_id": "m8MDnv9okwxFNBV"
-- }




-- GET TRANSAXCTIONS



data TransactionsRequest = TransactionsRequest
    { clientId :: Id Client
    , secret :: Id Secret
    , accessToken :: Token Access
    , startDate :: Day
    , endDate :: Day
    , options :: ListRequestOptions
    } deriving (Generic, Show, Eq)

instance ToJSON TransactionsRequest


data ListRequestOptions = ListRequestOptions
    { count :: Int
    , offset :: Int
    , accountIds :: Maybe [ Id Account ]
    } deriving (Generic, Show, Eq)

instance ToJSON ListRequestOptions


data TransactionsResponse = TransactionsResponse
    { accounts :: [ Account ]
    , transactions :: [ Transaction ]
    , item :: Item
    , totalTransactions :: Int
    , requestId :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON ListRequestOptions

-- "location": { "address": "300 Post St", "city": "San Francisco", "state": "CA", "zip": "94108", "lat": null, "lon": null


data TransactionType = Digital | Place | Special | Unresolved
    deriving (Show, Eq, Generic)

instance FromJSON TransactionType where
    parseJSON = genericParseJSON enumOptions


newtype Category = Category Text
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


data Transaction = Transaction
    { accountId :: Id Account
    , amount :: Currency
    , isoCurrencyCode :: Maybe CurrencyCode
    , unofficialCurrencyCode :: Maybe CurrencyCode
    , category :: Maybe [ Category ]
    , categoryId :: Maybe (Id Category)
    , date :: Day
    , location :: Location
    , name :: Text
    , pending :: Bool
    , pendingTransactionId :: Maybe Text
    , transactionId :: Id Transaction
    , transactionType :: TransactionType
    -- , "payment_meta": Object,
    -- , "account_owner": null,
    } deriving (Generic, Show, Eq)

instance FromJSON Transaction


data Location = Location
    { address :: Maybe Text
    , city :: Maybe Text
    , state :: Maybe Text
    , zip :: Maybe Text
    , lat :: Maybe Float
    , lon :: Maybe Float
    } deriving (Generic, Show, Eq)

instance FromJSON Location


-- curl -X POST https://sandbox.plaid.com/transactions/get \
-- -H 'Content-Type: application/json' \
-- -d '{
--   "client_id": String,
--   "secret": String,
--   "access_token": String,
--   "start_date": "2018-01-01",
--   "end_date": "2018-02-01",
--   "options": {
--     "count": 250,
--     "offset": 100
--   }
-- }'

-- http code 200
-- {
--  "accounts": [{object}],
--  "transactions": [{
--     "account_id": "vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d",
--     "amount": 2307.21,
--     "iso_currency_code": "USD",
--     "unofficial_currency_code": null,
--     "category": [
--       "Shops",
--       "Computers and Electronics"
--     ],
--     "category_id": "19013000",
--     "date": "2017-01-29",
--     "location": {
--      "address": "300 Post St",
--      "city": "San Francisco",
--      "state": "CA",
--      "zip": "94108",
--      "lat": null,
--      "lon": null
--     },
--     "name": "Apple Store",
--     "payment_meta": Object,
--     "pending": false,
--     "pending_transaction_id": null,
--     "account_owner": null,
--     "transaction_id": "lPNjeW1nR6CDn5okmGQ6hEpMo4lLNoSrzqDje",
--     "transaction_type": "place"
--    }, {
--     "account_id": "XA96y1wW3xS7wKyEdbRzFkpZov6x1ohxMXwep",
--     "amount": 78.5,
--     "iso_currency_code": "USD",
--     "unofficial_currency_code": null,
--     "category": [
--       "Food and Drink",
--       "Restaurants"
--     ],
--     "category_id": "13005000",
--     "date": "2017-01-29",
--     "location": {
--       "address": "262 W 15th St",
--       "city": "New York",
--       "state": "NY",
--       "zip": "10011",
--       "lat": 40.740352,
--       "lon": -74.001761
--     },
--     "name": "Golden Crepes",
--     "payment_meta": Object,
--     "pending": false,
--     "pending_transaction_id": null,
--     "account_owner": null,
--     "transaction_id": "4WPD9vV5A1cogJwyQ5kVFB3vPEmpXPS3qvjXQ",
--     "transaction_type": "place"
--   }],
--   "item": {Object},
--   "total_transactions": Number,
--   "request_id": "45QSn"
-- }



-- GET BALANCE

-- curl -X POST https://sandbox.plaid.com/accounts/balance/get \
-- -H 'Content-Type: application/json' \
-- -d '{
--   "client_id": String,
--   "secret": String,
--   "access_token": String,
--   "options": {
--     "account_ids": [String]
--   }
-- }'

-- http code 200
-- {
--   "accounts": [{
--      "account_id": "QKKzevvp33HxPWpoqn6rI13BxW4awNSjnw4xv",
--      "balances": {
--        "available": 100,
--        "current": 110,
--        "limit": null,
--        "iso_currency_code": "USD",
--        "unofficial_currency_code": null
--      },
--      "mask": "0000",
--      "name": "Plaid Checking",
--      "official_name": "Plaid Gold Checking",
--      "subtype": "checking",
--      "type": "depository"
--   }],
--   "item": {object},
--   "request_id": "m8MDnv9okwxFNBV"
-- }



-- Errors: 400s for developer, 500 for plaid
-- {
--   "error_type": String,
--   "error_code": String,
--   "error_message": String,
--   "display_message": String
-- }
