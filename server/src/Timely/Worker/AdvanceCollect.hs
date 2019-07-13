{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AdvanceCollect where


-- let's collect anything that's due today, in local time
-- anything less than or equal to the current time.
-- yeah, that makes sens. it's "due"

import           Control.Effects         (MonadEffects)
import           Control.Effects.Log     as Log
import           Control.Effects.Time    (Time)
import qualified Control.Effects.Time    as Time
import           Control.Effects.Worker  (Publish)
import qualified Control.Effects.Worker  as Worker
import           Control.Monad.Catch     (MonadThrow (..))
import           Data.Model.Guid         as Guid
import qualified Network.AMQP.Worker     as Worker hiding (publish)
import           Timely.Advances         (Advance (..), Advances)
import qualified Timely.Advances         as Advances
import qualified Timely.Advances.Collect as Collect
import           Timely.App              as App (runApp, start, appIO)
import           Timely.Events           as Events
import           Timely.Transfers        (Transfers)
import qualified Timely.Transfers        as Transfers



queue :: Worker.Queue Advance
queue = Worker.topic Events.advancesDue "app.advances.collect"

start :: IO ()
start = App.start runApp queue handler

startSchedule :: IO ()
startSchedule = App.appIO schedule


-- | Scans for due advances and queues them. Run this every hour, or every day at UTC midnight (or just after)
schedule
  :: ( MonadEffects '[Time, Log, Advances, Publish] m
     , MonadThrow m
     )
  => m ()
schedule = do
    Log.context "Schedule AdvanceCollect"
    now <- Time.currentTime
    let dueDate = Collect.currentlyDue now
    advances <- Advances.findDue dueDate
    mapM_ scheduleAdvanceCollect advances
  where
    scheduleAdvanceCollect a = do
      Log.info $ Guid.toText $ advanceId a
      Worker.publish Events.advancesDue a




handler
  :: ( MonadThrow m
     , MonadEffects '[Log, Advances, Transfers] m
     )
  => Advance -> m ()
handler advance = do
  Log.context (Guid.toText $ advanceId advance)

  t <- Transfers.debit advance
  Log.debug ("Transfer Debit", t)

  Advances.markCollected (advanceId advance)
  Log.info "collected"
