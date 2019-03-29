{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Timely.Accounts
  ( Accounts(..)
  , implementIO
  , initialize

  , all
  , find
  , findByPhone
  , findByBankId
  , findBanks
  , setHealth
  , create
  , setBanks
  , saveTransactions
  , listTransactions

  , module Timely.Accounts.Types
  ) where


import           Control.Effects              (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda          (Selda, tryCreateTable)
import           Data.Model.Guid
import           Data.Model.Id                (Id)
import           Data.Model.Types             (Phone)
import           Data.Model.Valid             (Valid)
import           Database.Selda               hiding (deleteFrom, insert, query, tryCreateTable)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (all)
import qualified Timely.Accounts.Account      as Account
import           Timely.Accounts.Transactions (Count, Offset)
import qualified Timely.Accounts.Transactions as Transactions
import           Timely.Accounts.Types
import           Timely.Bank                  (Item)
import           Timely.Evaluate.Types        (Projection (..))




data Accounts m = AccountsMethods
    { _all              :: m [Account]
    , _find             :: Guid Account -> m (Maybe Account)
    , _findByPhone      :: Valid Phone -> m (Maybe Account)
    , _findByBankId     :: Id Item -> m [Account]
    , _create           :: Account -> Customer -> [BankAccount] -> [Transaction] -> m ()
    , _setHealth        :: Guid Account -> Projection -> m ()
    , _findBanks        :: Guid Account -> m [BankAccount]
    , _setBanks         :: Guid Account -> [BankAccount] -> m ()

    , _saveTransactions :: Guid Account -> [Transaction] -> m ()
    , _listTransactions :: Guid Account -> Offset -> Count -> m [Transaction]
    } deriving (Generic)










instance Effect Accounts

all          :: MonadEffect Accounts m => m [Account]
find         :: MonadEffect Accounts m => Guid Account -> m (Maybe Account)
findByPhone  :: MonadEffect Accounts m => Valid Phone -> m (Maybe Account)
findByBankId :: MonadEffect Accounts m => Id Item     -> m [Account]
findBanks    :: MonadEffect Accounts m => Guid Account -> m [BankAccount]
setHealth    :: MonadEffect Accounts m => Guid Account -> Projection -> m ()
create       :: MonadEffect Accounts m => Account -> Customer -> [BankAccount] -> [Transaction] -> m ()
setBanks     :: MonadEffect Accounts m => Guid Account -> [BankAccount] -> m ()
saveTransactions :: MonadEffect Accounts m => Guid Account -> [Transaction] -> m ()
listTransactions :: MonadEffect Accounts m => Guid Account -> Offset -> Count -> m [Transaction]
AccountsMethods all find findByPhone findByBankId create setHealth findBanks setBanks saveTransactions listTransactions = effect



implementIO :: Selda m => RuntimeImplemented Accounts m a -> m a
implementIO = implement $
  AccountsMethods
    Account.allAccounts
    Account.getAccount
    Account.getAccountIdByPhone
    Account.getAccountByBankId
    createAccount
    Account.setAccountHealth
    Account.getBankAccounts
    Account.setBankAccounts
    Transactions.save
    Transactions.list
  where
    createAccount acc@Account{accountId} cust banks trans = do
      Account.createAccount acc cust
      Account.setBankAccounts accountId banks
      Transactions.save accountId trans



initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable Account.accounts
    tryCreateTable Account.customers
    tryCreateTable Account.banks
    tryCreateTable Account.healths
    tryCreateTable Transactions.transactions
