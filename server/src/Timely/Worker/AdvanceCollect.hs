{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AdvanceCollect where


-- let's collect anything that's due today, in local time
-- anything less than or equal to the current time.
-- yeah, that makes sens. it's "due"

import           Control.Effects           (MonadEffects)
import           Control.Effects.Log       as Log
import           Control.Effects.Time      (Time)
import qualified Control.Effects.Time      as Time
import           Control.Monad.Catch       (MonadThrow (..))
import           Control.Monad.Service     (Service (run))
-- import           Data.String.Conversions   (cs)
import           Data.Model.Guid           as Guid
import qualified Network.AMQP.Worker       as Worker hiding (publish)
import           Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as Worker

import           Timely.Advances           (Advance (..), Advances)
import qualified Timely.Advances           as Advances
import qualified Timely.Advances.Collect   as Collect
import           Timely.Events             as Events
import           Timely.Transfers          (Transfers)
import qualified Timely.Transfers          as Transfers



queue :: Worker.Queue Advance
queue = Worker.topic Events.advancesDue "app.advances.collect"



-- | Scans for due advances and queues them. Run this every hour, or every day at UTC midnight (or just after)
schedule
  :: ( Service m Advances
     , MonadEffects '[Time, Log] m
     , MonadThrow m
     , MonadWorker m
     )
  => m ()
schedule = do
    Log.context "Schedule AdvanceCollect"
    now <- Time.currentTime
    let dueDate = Collect.currentlyDue now
    advances <- run $ Advances.FindDue dueDate
    mapM_ scheduleAdvanceCollect advances
  where
    scheduleAdvanceCollect a = do
      Log.info $ Guid.toText $ advanceId a
      Worker.publish Events.advancesDue a




handler
  :: ( Service m Advances
     , Service m Transfers
     , MonadThrow m
     , MonadEffects '[Log] m
     )
  => Advance -> m ()
handler advance = do
  Log.context (Guid.toText $ advanceId advance)

  t <- Transfers.debit advance
  Log.debug ("Transfer Debit", t)

  run $ Advances.MarkCollected (advanceId advance)
  Log.info "collected"
