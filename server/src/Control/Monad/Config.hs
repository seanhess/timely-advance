{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Config where

import Control.Monad.Reader (ReaderT, ask)

class Monad m => MonadConfig c m where
  config :: m c


instance Monad m => MonadConfig c (ReaderT c m) where
  config = ask



configs :: MonadConfig c m => (c -> a) -> m a
configs f = do
  cfg <- config
  pure $ f cfg




