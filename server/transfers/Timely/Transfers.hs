{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Timely.Transfers
  ( Store.initialize
  , Transfers(..)
  , ACH.Config(..)
  , TransferAccount
  , AccountInfo(..)
  , credit
  , debit
  ) where

import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.Config     (MonadConfig (..))
import           Control.Monad.Selda      (Selda)
import           Control.Monad.Service    (Service (..))
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




data Transfers a where
    ACHSend    :: Id TransferAccount -> Transfer Credit -> Transfers ()
    ACHCollect :: Id TransferAccount -> Transfer Debit -> Transfers ()

    CreateAccount  :: AccountInfo -> Transfers (Id TransferAccount)

    SaveCredit :: Advance -> Transfers (Transfer Credit)
    SaveDebit  :: Advance -> Transfers (Transfer Debit)
    MarkSuccess :: (Typeable s, TransferStore s) => Transfer s -> Transfers ()



instance (MonadThrow m, Selda m, MonadConfig Config m) => Service m Transfers where
  run (SaveCredit a)    = Store.saveCredit a
  run (SaveDebit a)     = Store.saveDebit a
  run (MarkSuccess t)   = Store.markSuccess t

  run (ACHSend i t)     = ACH.sendMoney i t
  run (ACHCollect i t)  = ACH.collectMoney i t
  run (CreateAccount a) = ACH.initAccount a



-- Main actions --------------------------------


credit :: (Service m Transfers) => Advance -> m (Transfer Credit)
credit a = do
  t <- run $ SaveCredit a
  run $ ACHSend (Advances.transferId a) t
  run $ MarkSuccess t
  pure t


debit :: (Service m Transfers) => Advance -> m (Transfer Debit)
debit a = do
  t <- run $ SaveDebit a
  run $ ACHCollect (Advances.transferId a) t
  run $ MarkSuccess t
  pure t

