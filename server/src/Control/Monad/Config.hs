{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Config where

class Monad m => MonadConfig c m where
  config :: m c



configs :: MonadConfig c m => (c -> a) -> m a
configs f = do
  cfg <- config
  pure $ f cfg



