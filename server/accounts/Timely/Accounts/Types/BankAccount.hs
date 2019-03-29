{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Timely.Accounts.Types.BankAccount where

import           Data.Aeson                    (ToJSON (..))
import           Data.Model.Guid               (Guid)
import           Data.Model.Id                 (Id (..))
import           Data.Model.Money              as Money
import           Data.Text                     as Text
import           Data.Time.Clock               (UTCTime)
import           Data.Typeable                 (Typeable)
import           Database.Selda                (ID, SqlRow (..), SqlType, def)
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
    { id            :: ID BankAccount
    , accountId     :: Guid Account
    , accountType   :: BankAccountType
    , bankAccountId :: Id Bank.Account
    , name          :: Text
    , balance       :: Money
    , created       :: UTCTime
    } deriving (Generic, Eq, Show)


instance SqlRow BankAccount
instance ToJSON BankAccount




toBankAccount :: Guid Account -> UTCTime -> Bank.Account -> BankAccount
toBankAccount accountId now acc = BankAccount {..}
  where
    id = def
    created = now
    accountType
      | Bank.subtype acc == Bank.Checking = Checking
      | Bank.subtype acc == Bank.Savings = Savings
      | Bank._type acc == Bank.Credit = Credit
      | Bank._type acc == Bank.Depository = Savings
      | Bank._type acc == Bank.Loan = Credit
      | otherwise = Other
    name = Bank.name (acc :: Bank.Account)
    balance = toBalance $ Bank.current $ Bank.balances acc
    toBalance (Bank.Currency d) = Money.fromFloat d
    bankAccountId = Bank.account_id (acc :: Bank.Account)


isChecking :: BankAccount -> Bool
isChecking acc = accountType acc == Checking
