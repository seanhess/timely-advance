{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Timely.Accounts.Types.BankAccount where

import           Data.Aeson                    (ToJSON (..), genericToJSON, defaultOptions)
import           Data.Model.Guid               (Guid)
import           Data.Model.Id                 (Id (..))
import           Data.Model.Money              as Money
import           Data.Text                     as Text
import           Data.Time.Clock               (UTCTime)
import           Data.Typeable                 (Typeable)
import           Database.Selda                (SqlRow (..), SqlType)
import           GHC.Generics                  (Generic)
import           Timely.Accounts.Types.Account (Account)
import           Timely.Accounts.Types.Api ()
import qualified Timely.Bank                   as Bank



newtype BankAccountType = BankAccountType { bankAccountType :: Bank.AccountType }
    deriving (Generic, Eq, Show, Bounded, Enum, Read, Typeable)

instance SqlType BankAccountType
instance ToJSON BankAccountType where
  toJSON (BankAccountType t) = genericToJSON defaultOptions t



data BankAccount = BankAccount
    { accountId     :: Guid Account
    , accountType   :: BankAccountType
    , bankAccountId :: Id Bank.Account
    , name          :: Text
    , balance       :: Money
    , created       :: UTCTime
    } deriving (Generic, Eq, Show)


instance SqlRow BankAccount
instance ToJSON BankAccount




toBankAccount :: Guid Account -> UTCTime -> Bank.Account -> BankAccount
toBankAccount accountId now acc =
  BankAccount
    { created = now
    , accountId = accountId
    , accountType = BankAccountType $ Bank.accountType acc
    , name = Bank.name (acc :: Bank.Account)
    , balance = toBalance $ Bank.balance acc
    , bankAccountId = Bank.accountId (acc :: Bank.Account)
    }

  where

    toBalance (Bank.Currency d) = Money.fromFloat d


isChecking :: BankAccount -> Bool
isChecking acc = bankAccountType (accountType acc) == Bank.Checking
