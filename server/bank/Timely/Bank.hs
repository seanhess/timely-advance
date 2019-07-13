{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
module Timely.Bank
    ( Token(..)
    , Id(..)
    , Access
    , Public
    , Account(..)
    , Currency(..)
    , AccountType(..)
    , Identity(..)
    , Names(..)
    , Identity.Address(..)
    , Identity.IdentityInfo(_data, _primary)
    , Identity.AddressInfo(..)
    , Banks(..)
    , Config(..)
    , Options(..)
    , Dwolla
    , Item
    , Transaction(..)
    , Category(..)
    , authenticate
    , loadIdentity -- remove me when you add it back in
    , loadAccounts
    , loadTransactions
    , loadTransactionsRange
    , loadTransactionsDays
    , getACH
    , implementIO
    , implementOfflineMock
    ) where

import           Control.Effects            (Effect (..), MonadEffect (..), MonadEffects, RuntimeImplemented, effect, implement)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Config       (MonadConfig)
import           Control.Monad.IO.Class     (MonadIO)
import qualified Control.Monad.Loops        as Loops
import qualified Data.List                  as List
import           Data.Model.Id              (Id (..), Token (..))
import           Data.Model.Types           (Address (..), Valid (..))
import           Data.Time.Calendar         (Day)
import qualified Data.Time.Calendar         as Day
import           GHC.Generics               (Generic)
import           Network.Plaid.Dwolla       (Dwolla)
import qualified Network.Plaid.Identity     as Identity
import           Network.Plaid.Transactions (Options (..))
import           Network.Plaid.Types        as Plaid (Access, Category (..), Currency (..), Item, Public, Transaction (..))
import qualified Timely.Bank.Actions        as Actions
import           Timely.Bank.Types          (Account (..), AccountType (..), Config (..), Identity (..), Names (..))

-- Bank Service

data Banks m = BanksMethods
    { _authenticate     :: Token Public -> m (Token Access, Id Item)
    , _loadIdentity     :: Token Access -> m Identity
    , _loadAccounts     :: Token Access -> m [Account]
    , _loadTransactions :: Token Access -> Id Account -> Options -> m [Transaction]
    , _getACH           :: Token Access -> Id Account -> m (Token Dwolla)
    } deriving (Generic)

instance Effect Banks

authenticate     :: MonadEffect Banks m => Token Public -> m (Token Access, Id Item)
loadIdentity     :: MonadEffect Banks m => Token Access -> m Identity
loadAccounts     :: MonadEffect Banks m => Token Access -> m [Account]
loadTransactions :: MonadEffect Banks m => Token Access -> Id Account -> Options -> m [Transaction]
getACH           :: MonadEffect Banks m => Token Access -> Id Account -> m (Token Dwolla)
BanksMethods authenticate loadIdentity loadAccounts loadTransactions getACH = effect






-- | loads all the transactions in the range, and continues to load them until it gets the full range
loadTransactionsRange :: MonadEffects '[Banks] m => Token Access -> Id Account -> Day -> Day -> m [Transaction]
loadTransactionsRange tok aid start end = do
  tss <- Loops.unfoldrM loadNext 0
  pure $ List.concat tss

  where
    loadNext offset = do
      res <- loadTransactions tok aid $ Options
               { start_date = start
               , end_date   = end
               , count = 500
               , offset = offset
               }

      case res of
        [] -> pure Nothing
        ts -> pure $ Just (ts, offset + 500)


loadTransactionsDays :: MonadEffects '[Banks] m => Token Access -> Id Account -> Integer -> Day -> m [Transaction]
loadTransactionsDays tok aid days today = do
  loadTransactionsRange tok aid (Day.addDays (-days) today) today






implementIO :: (MonadIO m, MonadThrow m, MonadConfig Config m) => RuntimeImplemented Banks m a -> m a
implementIO =
  implement $
    BanksMethods
      Actions.authenticate
      Actions.loadIdentity
      Actions.loadAccounts
      Actions.loadTransactions
      Actions.getACH

implementOfflineMock :: (MonadIO m, MonadThrow m, MonadConfig Config m) => RuntimeImplemented Banks m a -> m a
implementOfflineMock = do
  let accountId = Id "bank-mock-id"
      accessToken = Token "bank-mock-access"
  implement $
    BanksMethods
      (\_ -> pure (accessToken, accountId))
      (\_ -> pure $ Identity (Names "Mock" Nothing "Person") (Address "1234 West Mock St" Nothing "Bigtown" (Valid "CA") (Valid "12345")))
      (\_ -> pure [Account accountId (Currency 125.00) "Mock Checking" Checking])
      Actions.loadTransactions
      Actions.getACH
