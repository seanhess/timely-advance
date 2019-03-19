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
    , CurrencyCode(..)
    , Balances(..)
    , AccountType(..)
    , AccountSubType(..)
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
    , loadTransactionsDays
    , limitLast
    , getACH
    , implementBankIO
    -- , runPlaid
    ) where

import           Control.Effects            (Effect (..), MonadEffect (..), MonadEffects, RuntimeImplemented, effect,
                                             implement)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Config       (MonadConfig)
import           Control.Monad.IO.Class     (MonadIO)
import qualified Control.Monad.Loops        as Loops
import qualified Data.List                  as List
import           Data.Model.Id              (Id (..), Token (..))
import           Data.Time.Calendar         (Day)
import qualified Data.Time.Calendar         as Day
import           GHC.Generics               (Generic)
import           Network.Plaid.Dwolla       (Dwolla)
import qualified Network.Plaid.Identity     as Identity
import           Network.Plaid.Transactions (Options (..))
import           Network.Plaid.Types
import qualified Timely.Bank.Actions        as Actions
import           Timely.Bank.Types          (Config (..), Identity (..), Names (..))

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




-- | Loads the most recent N transactions (N < 500)
limitLast :: Day -> Int -> Options
limitLast today num = Options
    { start_date = Day.addDays (-365) today
    , end_date   = Day.addDays (1) today
    , count      = num
    , offset     = 0
    }



-- | loads all the transactions in the range, and continues to load them until it gets the full range
loadTransactionsDays :: MonadEffects '[Banks] m => Token Access -> Id Account -> Day -> Integer -> m [Transaction]
loadTransactionsDays tok aid today days = do
  tss <- Loops.unfoldrM loadNext 0
  pure $ List.concat tss

  where
    loadNext offset = do
      res <- loadTransactions tok aid $ Options
               { start_date = Day.addDays (-days) today
               , end_date   = Day.addDays (1) today
               , count = 500
               , offset = offset
               }

      case res of
        [] -> pure Nothing
        ts -> pure $ Just (ts, offset + 500)







-- this makes it so you can mock out each individual effect
-- if you replace it with mocking runPlaid, it's harder to mock for tests
implementBankIO :: (MonadIO m, MonadThrow m, MonadConfig Config m) => RuntimeImplemented Banks m a -> m a
implementBankIO =
  implement $
    BanksMethods
      Actions.authenticate
      Actions.loadIdentity
      Actions.loadAccounts
      Actions.loadTransactions
      Actions.getACH
