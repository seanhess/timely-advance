{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Underwriting
  ( Underwriting(..)
  , newCustomer
  , implementMock
  , module Timely.Underwriting.Types
  ) where

import           Control.Effects           (Effect (..), MonadEffect (..), RuntimeImplemented)
import qualified Control.Effects           as Effects
import qualified Data.Model.Money          as Money
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Customer (..))
import           Timely.Underwriting.Types



data Underwriting m = UnderwritingMethods
    -- we need a lot more information than this
    -- TODO storage? Save everything
    { _newCustomer :: Customer -> m Result
    } deriving (Generic)

instance Effect Underwriting


newCustomer :: MonadEffect Underwriting m => Customer -> m Result
newCustomer = _newCustomer effect




implementMock :: Monad m => RuntimeImplemented Underwriting m a -> m a
implementMock =
  Effects.implement $
    UnderwritingMethods mockUnderwrite


-- instance Monad m => Service m Underwriting where
--     run (New a) = underwrite a


mockUnderwrite :: Monad m => Customer -> m Result
mockUnderwrite app = pure $ mock $ email app
  where
    mock "100"    = Approved $ Approval $ Money.fromFloat 100.00
    mock "200"    = Approved $ Approval $ Money.fromFloat 200.00
    mock "300"    = Approved $ Approval $ Money.fromFloat 300.00
    mock "400"    = Approved $ Approval $ Money.fromFloat 400.00
    mock "denied" = Denied $ Denial NoReason
    mock _        = Approved $ Approval $ Money.fromFloat 250.00
