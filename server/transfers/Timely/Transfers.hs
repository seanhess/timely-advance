{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
module Timely.Transfers
  ( Store.initialize
  , Transfers(..)
  , ACH.Config(..)
  , TransferAccount
  , AccountInfo(..)
  , credit
  , debit
  , createAccount
  , implementIO
  ) where

import           Control.Effects          (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.Config     (MonadConfig (..))
import           Control.Monad.Selda      (Selda)
import           Control.Monad.Trans      (lift)
import           Data.Model.Id            (Id)
import           Data.Typeable            (Typeable)
import           Timely.Transfers.Account
import           Timely.Transfers.ACH     (Config)
import qualified Timely.Transfers.ACH     as ACH
import           Timely.Transfers.Store   (TransferStore)
import qualified Timely.Transfers.Store   as Store
import           Timely.Transfers.Types

import           Timely.Advances          (Advance (..))
import qualified Timely.Advances          as Advances




data Transfers m = TransfersMethods
  { _achSend       :: Id TransferAccount -> Transfer Credit -> m ()
  , _achCollect    :: Id TransferAccount -> Transfer Debit -> m ()

  , _createAccount :: AccountInfo -> m (Id TransferAccount)

  , _saveCredit    :: Advance -> m (Transfer Credit)
  , _saveDebit     :: Advance -> m (Transfer Debit)
  , _markSuccess   :: forall a. (Typeable a, TransferStore a) => Transfer a -> m ()
  }



achSend           :: MonadEffect Transfers m => Id TransferAccount -> Transfer Credit -> m ()
achCollect        :: MonadEffect Transfers m => Id TransferAccount -> Transfer Debit -> m ()

createAccount     :: MonadEffect Transfers m => AccountInfo -> m (Id TransferAccount)

saveCredit        :: MonadEffect Transfers m => Advance -> m (Transfer Credit)
saveDebit         :: MonadEffect Transfers m => Advance -> m (Transfer Debit)
TransfersMethods achSend achCollect createAccount saveCredit saveDebit _ = effect

markSuccess       :: forall a m. (Typeable a, TransferStore a, MonadEffect Transfers m) => Transfer a -> m ()
markSuccess = _markSuccess effect




implementIO :: (Selda m, MonadThrow m, MonadConfig Config m) => RuntimeImplemented Transfers m a -> m a
implementIO =
  implement $
    TransfersMethods
      ACH.sendMoney
      ACH.collectMoney
      ACH.initAccount
      Store.saveCredit
      Store.saveDebit
      Store.markSuccess





-- Main actions --------------------------------


credit :: MonadEffect Transfers m => Advance -> m (Transfer Credit)
credit a = do
  t <- saveCredit a
  achSend (Advances.transferId a) t
  markSuccess t
  pure t


debit :: MonadEffect Transfers m => Advance -> m (Transfer Debit)
debit a = do
  t <- saveDebit a
  achCollect (Advances.transferId a) t
  markSuccess t
  pure t





--- Boilerplate ----------------------------

instance Effect Transfers where
  liftThrough methods = TransfersMethods
    (\i t -> lift (_achSend methods i t))
    (\i t -> lift (_achCollect methods i t))
    (\a   -> lift (_createAccount methods a))
    (\a   -> lift (_saveCredit methods a))
    (\a   -> lift (_saveDebit methods a))
    (\t   -> lift (_markSuccess methods t))

  mergeContext tm = TransfersMethods
    (\i t -> do
        m <- tm
        _achSend m i t)
    (\i t -> do
        m <- tm
        _achCollect m i t)
    (\a   -> do
        m <- tm
        _createAccount m a)
    (\a   -> do
        m <- tm
        _saveCredit m a)
    (\a   -> do
        m <- tm
        _saveDebit m a)
    (\t   -> do
        m <- tm
        _markSuccess m t)
