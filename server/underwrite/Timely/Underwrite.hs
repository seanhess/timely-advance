{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Underwrite
  ( Underwrite(..)
  , new
  , implementMock
  , module Timely.Underwrite.Types
  ) where

import           Control.Effects           (Effect (..), MonadEffect (..), RuntimeImplemented)
import qualified Control.Effects           as Effects
import qualified Data.Model.Money          as Money
import           GHC.Generics              (Generic)
import           Timely.Underwrite.Types



data Underwrite m = UnderwriteMethods
    -- we need a lot more information than this
    -- TODO storage? Save everything
    { _new :: Application -> m Result
    } deriving (Generic)

instance Effect Underwrite


new :: MonadEffect Underwrite m => Application -> m Result
new = _new effect




implementMock :: Monad m => RuntimeImplemented Underwrite m a -> m a
implementMock =
  Effects.implement $
    UnderwriteMethods mockUnderwrite


-- instance Monad m => Service m Underwrite where
--     run (New a) = underwrite a


mockUnderwrite :: Monad m => Application -> m Result
mockUnderwrite app = pure $ mock $ email app
  where
    mock "100"    = Approved $ Approval $ Money.fromFloat 100.00
    mock "200"    = Approved $ Approval $ Money.fromFloat 200.00
    mock "300"    = Approved $ Approval $ Money.fromFloat 300.00
    mock "400"    = Approved $ Approval $ Money.fromFloat 400.00
    mock "denied" = Denied $ Denial NoReason
    mock _        = Approved $ Approval $ Money.fromFloat 250.00
