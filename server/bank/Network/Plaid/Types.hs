{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Network.Plaid.Types where

import           Data.Aeson         (FromJSON, Options (..), ToJSON, Value, defaultOptions, genericParseJSON, parseJSON,
                                     withText)
import           Data.Char          (toLower)
import           Data.Model.Id      (Id (..), Token (..))
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)



{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


data Credentials = Credentials
    { client_id :: Id Client
    , secret    :: Id Secret
    } deriving (Generic, Show, Eq)













-- Errors: 400s for developer, 500 for plaid
-- {
--   "error_type": String,
--   "error_code": String,
--   "error_message": String,
--   "display_message": String
-- }











-- * General Types

-- Token types
data Public
data Access = Access
    { client_id    :: Id Client
    , secret       :: Id Secret
    , access_token :: Token Access
    }

-- Id Types
data Client
data Secret
-- data Request


-- Number
data Routing
data WireRouting
data Institution
data Branch


newtype Number t = Number Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq)

newtype Item = Item Value
    deriving (Generic, FromJSON, ToJSON, Show, Eq)


newtype Currency = Currency Float
    deriving (Generic, FromJSON, ToJSON, Show, Eq)

newtype CurrencyCode = CurrencyCode Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq)





-- * Account Types


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
      subType "savings"  = Savings
      subType t          = SubType t

data Account = Account
    { account_id    :: Id Account
    , balances      :: Balances
    , mask          :: Text
    , name          :: Text
    , official_name :: Text
    , subtype       :: AccountSubType
    , _type         :: AccountType
    } deriving (Generic, Show, Eq)

instance FromJSON Account where
    parseJSON = genericParseJSON defaultOptions
                  { fieldLabelModifier = dropWhile (== '_') }

data Balances = Balances
    { current                  :: Currency
    , available                :: Maybe Currency
    , iso_currency_code        :: Maybe CurrencyCode
    , unofficial_currency_code :: Maybe CurrencyCode
    } deriving (Generic, Show, Eq)

instance FromJSON Balances


enumOptions :: Options
enumOptions = defaultOptions { constructorTagModifier = map toLower }











-- * Auth Types




-- * Transaction Types

data TransactionType = Digital | Place | Special | Unresolved
    deriving (Show, Eq, Generic)

instance FromJSON TransactionType where
    parseJSON = genericParseJSON enumOptions


newtype Category = Category Text
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


data Transaction = Transaction
    { account_id               :: Id Account
    , amount                   :: Currency
    , iso_currency_code        :: Maybe CurrencyCode
    , unofficial_currency_code :: Maybe CurrencyCode
    , category                 :: Maybe [ Category ]
    , category_id              :: Maybe (Id Category)
    , date                     :: Day
    , location                 :: Location
    , name                     :: Text
    , pending                  :: Bool
    , pending_transaction_id   :: Maybe Text
    , transaction_id           :: Id Transaction
    , transaction_type         :: TransactionType
    -- , "payment_meta": Object,
    -- , "account_owner": null,
    } deriving (Generic, Show, Eq)

instance FromJSON Transaction


data Location = Location
    { address :: Maybe Text
    , city    :: Maybe Text
    , state   :: Maybe Text
    , zip     :: Maybe Text
    , lat     :: Maybe Float
    , lon     :: Maybe Float
    } deriving (Generic, Show, Eq)

instance FromJSON Location









