module Timely.Worker.Schedule where

import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.Async.Timer as Timer
import           Control.Monad                  (forever)
import           Data.Function                  ((&))
import qualified Timely.Worker.AccountUpdate    as AccountUpdate
import qualified Timely.Worker.AdvanceCollect   as AdvanceCollect
import qualified Timely.Worker.WorkerM          as Worker


start :: IO ()
start = do
  putStrLn "STARTING SCHEDULER"
  -- TODO account update shouldn't be run every minute. Every hour in production, or use the webhook
  -- we shouldn't do it this way for production
  Async.mapConcurrently_ id
    [ every minute $ Worker.runIO AdvanceCollect.schedule
    , every minute $ Worker.runIO AccountUpdate.schedule
    ]







repeatM :: Timer.TimerConf -> IO () -> IO ()
repeatM conf action = do
  Timer.withAsyncTimer conf $ \timer -> do
    forever $ do
      Timer.wait timer
      action


type Seconds = Int

every :: Seconds -> IO () -> IO ()
every interval action =
  repeatM (Timer.defaultConf & Timer.setInterval interval) action


second = 1000
minute = 60 * second
hour   = 60 * minute
