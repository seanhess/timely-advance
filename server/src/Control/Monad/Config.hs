module Control.Monad.Config where

class MonadConfig c m where
  config :: m c



