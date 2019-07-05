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
  , empty

  , all
  , find
  , findCustomer
  , findByPhone
  , findByBankId
  , findBanks
  , create
  , setBanks
  , transSave
  , transList
  , transSince
  , transDays
  , subSave
  , subFind
  , subRemove

  , module Timely.Accounts.Types
  ) where


import           Control.Effects              (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda          (Selda, tryCreateTable)
import           Data.Model.Guid
import           Data.Model.Id                (Id)
import           Data.Model.Types             (Phone)
import           Data.Model.Valid             (Valid)
import           Data.Time.Calendar           (Day, addDays)
import           Database.Selda               hiding (deleteFrom, insert, query, tryCreateTable)
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (all)
import qualified Timely.Accounts.Account      as Account
import           Timely.Accounts.Transactions (Count, Offset)
import qualified Timely.Accounts.Transactions as Transactions
import qualified Timely.Accounts.Subscription as Subscription
import           Timely.Accounts.Types
import           Timely.Bank                  (Item)




data Accounts m = AccountsMethods
    { _all          :: m [AccountCustomer]
    , _find         :: Guid Account -> m (Maybe Account)
    , _findCustomer :: Guid Account -> m (Maybe Customer)
    , _findByPhone  :: Valid Phone -> m (Maybe Account)
    , _findByBankId :: Id Item -> m [Account]
    , _create       :: Customer -> [BankAccount] -> [TransactionRow] -> Subscription.Level -> Account -> m ()
    , _findBanks    :: Guid Account -> m [BankAccount]
    , _setBanks     :: Guid Account -> [BankAccount] -> m ()
    , _transSave    :: Guid Account -> [TransactionRow] -> m ()
    , _transList    :: Guid Account -> Offset -> Count -> m [TransactionRow]
    , _transSince   :: Guid Account -> Day -> m [TransactionRow]
    , _subFind      :: Guid Account -> m (Maybe Subscription)
    , _subSave      :: Guid Account -> Subscription -> m ()
    , _subRemove    :: Guid Account -> m ()
    } deriving (Generic)










instance Effect Accounts

all          :: MonadEffect Accounts m => m [AccountCustomer]
find         :: MonadEffect Accounts m => Guid Account -> m (Maybe Account)
findCustomer :: MonadEffect Accounts m => Guid Account -> m (Maybe Customer)
findByPhone  :: MonadEffect Accounts m => Valid Phone -> m (Maybe Account)
findByBankId :: MonadEffect Accounts m => Id Item     -> m [Account]
findBanks    :: MonadEffect Accounts m => Guid Account -> m [BankAccount]
create       :: MonadEffect Accounts m => Customer -> [BankAccount] -> [TransactionRow] -> Subscription.Level -> Account -> m ()
setBanks     :: MonadEffect Accounts m => Guid Account -> [BankAccount] -> m ()
transSave :: MonadEffect Accounts m => Guid Account -> [TransactionRow] -> m ()
transList :: MonadEffect Accounts m => Guid Account -> Offset -> Count -> m [TransactionRow]
transSince :: MonadEffect Accounts m => Guid Account -> Day -> m [TransactionRow]
subFind      :: MonadEffect Accounts m => Guid Account -> m (Maybe Subscription)
subSave      :: MonadEffect Accounts m => Guid Account -> Subscription -> m ()
subRemove    :: MonadEffect Accounts m => Guid Account -> m ()
AccountsMethods all find findCustomer findByPhone findByBankId create findBanks setBanks transSave transList transSince subFind subSave subRemove = effect



implementIO :: Selda m => RuntimeImplemented Accounts m a -> m a
implementIO = implement $
  AccountsMethods
    Account.allAccounts
    Account.getAccount
    Account.getCustomer
    Account.getAccountIdByPhone
    Account.getAccountByBankId
    createAccount
    Account.getBankAccounts
    Account.setBankAccounts
    Transactions.save
    Transactions.list
    Transactions.since
    Subscription.find
    Subscription.save
    Subscription.remove
  where
    createAccount cust banks trans subLevel acc@Account{accountId} = do
      Account.createAccount acc cust
      Account.setBankAccounts accountId banks
      Transactions.save accountId trans
      Subscription.save accountId $ Subscription.fromLevel subLevel


empty :: Accounts m
empty = AccountsMethods
  undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined



transDays :: MonadEffect Accounts m => Guid Account -> Integer -> Day -> m [TransactionRow]
transDays i num today =
  transSince i (addDays (-num) today)


initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable Account.accounts
    tryCreateTable Account.customers
    tryCreateTable Account.banks
    tryCreateTable Transactions.transactions
