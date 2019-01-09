{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Selda
  ( Selda(..)
  , insert
  , query
  , deleteFrom
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Database.Selda as Selda
import Database.Selda hiding (insert, query, deleteFrom)
import Database.Selda.Backend (runSeldaT, SeldaConnection)


class (MonadIO m, MonadMask m) => Selda m where
    withConnection :: (SeldaConnection -> m a) -> m a

query :: (Selda m, Result a) => Query s a -> m [Res a]
query q = withConnection $ runSeldaT (Selda.query q)

insert :: (Selda m, Relational a) => Table a -> [a] -> m Int
insert t vs = withConnection $ runSeldaT (Selda.insert t vs)

deleteFrom :: (Selda m, Relational a) => Table a -> (Row s a -> Col s Bool) -> m Int
deleteFrom t p = withConnection $ runSeldaT (Selda.deleteFrom t p)

