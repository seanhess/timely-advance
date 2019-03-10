{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
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
    , Dwolla
    , authenticate
    , loadIdentity -- remove me when you add it back in
    , loadAccounts
    , loadTransactions
    , getACH
    , implementBankIO
    -- , runPlaid
    ) where

import           Control.Effects        (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.Config   (MonadConfig)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Model.Id          (Id (..), Token (..))
import           GHC.Generics           (Generic)
import           Network.Plaid.Dwolla   (Dwolla)
import qualified Network.Plaid.Identity as Identity
import           Network.Plaid.Types
import qualified Timely.Bank.Actions    as Actions
import           Timely.Bank.Types      (Config (..), Identity (..), Names (..))

-- Bank Service

data Banks m = BanksMethods
    { _authenticate     :: Token Public -> m (Token Access)
    , _loadIdentity     :: Token Access -> m Identity
    , _loadAccounts     :: Token Access -> m [Account]
    , _loadTransactions :: Token Access -> Id Account -> m [Transaction]
    , _getACH           :: Token Access -> Id Account -> m (Token Dwolla)
    } deriving (Generic)

instance Effect Banks

authenticate     :: MonadEffect Banks m => Token Public -> m (Token Access)
loadIdentity     :: MonadEffect Banks m => Token Access -> m Identity
loadAccounts     :: MonadEffect Banks m => Token Access -> m [Account]
loadTransactions :: MonadEffect Banks m => Token Access -> Id Account -> m [Transaction]
getACH           :: MonadEffect Banks m => Token Access -> Id Account -> m (Token Dwolla)
BanksMethods authenticate loadIdentity loadAccounts loadTransactions getACH = effect


implementBankIO :: (MonadIO m, MonadThrow m, MonadConfig Config m) => RuntimeImplemented Banks m a -> m a
implementBankIO =
  implement $
    BanksMethods
      Actions.authenticate
      Actions.loadIdentity
      Actions.loadAccounts
      Actions.loadTransactions
      Actions.getACH
