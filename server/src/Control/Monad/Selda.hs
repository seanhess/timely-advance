{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Selda
  ( Selda(..)
  , insert
  , query
  , deleteFrom
  , tryCreateTable
  , update
  , update_
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Database.Selda as Selda
import Database.Selda (MonadMask)
import Database.Selda.Backend (runSeldaT, SeldaConnection)


class (MonadIO m, MonadMask m) => Selda m where
    withConnection :: (SeldaConnection -> m a) -> m a

query q          = withConnection $ runSeldaT $ Selda.query q
insert t vs      = withConnection $ runSeldaT $ Selda.insert t vs
deleteFrom t p   = withConnection $ runSeldaT $ Selda.deleteFrom t p
update t p f     = withConnection $ runSeldaT $ Selda.update t p f
update_ t p f    = withConnection $ runSeldaT $ Selda.update_ t p f
tryCreateTable t = withConnection $ runSeldaT $ Selda.tryCreateTable t
