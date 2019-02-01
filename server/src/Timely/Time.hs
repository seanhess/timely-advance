{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Timely.Time
  ( UTCTime(..)
  , Day
  , Time(..)
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Service  (Service (..))
import           Data.Time.Calendar     (Day)
import           Data.Time.Clock        (UTCTime (..), getCurrentTime)

data Time a where
    CurrentTime :: Time UTCTime
    CurrentDate :: Time Day

instance (MonadIO m) => Service m Time where
    run CurrentTime = liftIO $ getCurrentTime
    run CurrentDate = do
      UTCTime d _ <- liftIO $ getCurrentTime
      pure d
