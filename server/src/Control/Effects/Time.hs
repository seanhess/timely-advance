{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Effects.Time
  ( UTCTime(..)
  , Day
  , Time(..)
  , currentTime
  , currentDate
  , implementTimeIO
  ) where

import           Control.Effects        (Effect (..), MonadEffect, RuntimeImplemented, effect, implement)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Time.Calendar     (Day)
import           Data.Time.Clock        (UTCTime (..), getCurrentTime)
import           GHC.Generics           (Generic)

data Time m = TimeMethods
  { _currentTime :: m UTCTime
  } deriving (Generic)

instance Effect Time


currentTime :: MonadEffect Time m => m UTCTime
currentTime = _currentTime effect


currentDate :: MonadEffect Time m => m Day
currentDate = utctDay <$> _currentTime effect


implementTimeIO :: MonadIO m => RuntimeImplemented Time m a -> m a
implementTimeIO = implement $ TimeMethods (liftIO $ getCurrentTime)
