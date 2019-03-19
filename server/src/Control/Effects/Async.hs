{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Effects.Async where

import           Control.Concurrent     (threadDelay)
import           Control.Effects        (Effect (..), MonadEffect, RuntimeImplemented, effect, implement)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Loops    (untilJust)
import           GHC.Generics           (Generic)


type Microseconds = Int

data Async m = AsyncMethods
  { _delay :: Microseconds -> m ()
  } deriving (Generic)

instance Effect Async


delay :: MonadEffect Async m => Microseconds -> m ()
delay = _delay effect


poll :: MonadEffect Async m => Microseconds -> m (Maybe a) -> m a
poll us action = untilJust $ do
    ma <- action
    case ma of
      Just a -> return $ Just a
      Nothing -> do
        delay us
        return Nothing



implementIO :: MonadIO m => RuntimeImplemented Async m a -> m a
implementIO = implement $ AsyncMethods
  (liftIO . threadDelay)



