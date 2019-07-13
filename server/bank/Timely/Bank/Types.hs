{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Bank.Types where

import           Control.Exception      (Exception)
import           Data.Model.Id          (Id (..))
import           Data.Model.Types       (Address (..))
import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (Manager)
import           Network.Plaid.Identity (AddressInfo (..))
import           Network.Plaid.Types    as Plaid (Credentials, Currency)
import qualified Network.Plaid.Types    as Plaid
import           Servant.Client         (BaseUrl, ServantError)

data Config = Config
    { manager     :: Manager
    , baseUrl     :: BaseUrl
    , credentials :: Credentials
    }




data BankError
    = BadName Text
    | NoNames
    | BadAddresses [AddressInfo]
    | PlaidError ServantError
    deriving (Eq, Show)

instance Exception BankError



data Identity = Identity
    { names   :: Names
    , address :: Address
    } deriving (Show, Eq)

data Names = Names
    { firstName  :: Text
    , middleName :: Maybe Text
    , lastName   :: Text
    } deriving (Show, Eq)


data Account = Account
    { accountId   :: Id Account
    , balance     :: Currency
    , name        :: Text
    , accountType :: AccountType
    } deriving (Show, Eq)


data AccountType
    = Checking
    | Savings
    | Credit
    | Other
    deriving (Generic, Eq, Show, Bounded, Enum, Read, Typeable)



toAccount :: Plaid.Account -> Account
toAccount Plaid.Account {name, balances, account_id, _type, subtype} =
  Account
    { accountType = accountType _type subtype
    , name = name
    , balance = Plaid.current balances
    , accountId = toId account_id
    }

  where


    accountType typ sub
      | sub == Plaid.Checking = Checking
      | sub == Plaid.Savings = Savings
      | typ == Plaid.Credit = Credit
      | typ == Plaid.Depository = Savings
      | typ == Plaid.Loan = Credit
      | otherwise = Other

toId :: Id Plaid.Account -> Id Account
toId (Id a) = Id a

fromId :: Id Account -> Id Plaid.Account
fromId (Id a) = Id a
