{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Timely.Accounts.Types.BankAccount where

import           Data.Aeson                    (ToJSON (..))
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



data BankAccountType
    = Checking
    | Savings
    | Credit
    | Other
    deriving (Generic, Eq, Show, Bounded, Enum, Read, Typeable)

instance SqlType BankAccountType
instance ToJSON BankAccountType



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
    , accountId
    , accountType = accountType acc
    , name = Bank.name (acc :: Bank.Account)
    , balance = toBalance $ Bank.current $ Bank.balances acc
    , bankAccountId = Bank.account_id (acc :: Bank.Account)
    }

  where

    toBalance (Bank.Currency d) = Money.fromFloat d

    accountType a
      | Bank.subtype a == Bank.Checking = Checking
      | Bank.subtype a == Bank.Savings = Savings
      | Bank._type a == Bank.Credit = Credit
      | Bank._type a == Bank.Depository = Savings
      | Bank._type a == Bank.Loan = Credit
      | otherwise = Other


isChecking :: BankAccount -> Bool
isChecking acc = accountType acc == Checking
