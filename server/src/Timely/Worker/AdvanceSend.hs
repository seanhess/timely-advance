{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AdvanceSend where

import           Control.Effects       (MonadEffects)
import           Control.Effects.Log   as Log
import           Data.Model.Guid       as Guid
import qualified Network.AMQP.Worker   as Worker hiding (publish)

import           Timely.Advances       (Advance (..))
import           Timely.Events         as Events
import           Timely.Transfers      (Transfers)
import qualified Timely.App                    as App
import qualified Timely.Transfers      as Transfers



queue :: Worker.Queue Advance
queue = Worker.topic Events.advancesActive "app.advances.send"


start :: IO ()
start = App.start queue handler




handler
  :: (MonadEffects '[Log, Transfers] m
     )
  => Advance -> m ()
handler advance = do
  Log.context (Guid.toText $ advanceId advance)
  Transfers.credit advance
  Log.info "sent"
  pure ()




-- testQueue :: Worker.Queue Text
-- testQueue = Worker.topic Events.test "app.test"


-- data Bad = Bad Text
--   deriving (Show, Eq)

-- instance Exception Bad


-- test
--   :: ( Service m Advances
--      , Service m Transfers
--      , MonadThrow m
--      , MonadLog m
--      )
--   => Text -> m ()
-- test asdf = do
--   Log.context asdf
--   Log.info "hello"
--   throwM $ Bad "NOPE"
--   pure ()
