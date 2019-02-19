module Timely.App where


import           Control.Monad          (when)
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Retry          as Retry
import           Data.Function          ((&))


limitedBackoff :: Retry.RetryPolicy
limitedBackoff = Retry.constantDelay (1000*1000) & Retry.limitRetriesByCumulativeDelay (20*1000*1000)


retry :: (MonadIO m, MonadMask m) => m a -> m a
retry action = Retry.recoverAll limitedBackoff $ \s -> do
  when (Retry.rsIterNumber s > 0) $
    liftIO $ print ("Retry", Retry.rsCumulativeDelay s)
  action
