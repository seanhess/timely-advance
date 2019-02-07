{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AdvanceSend where

import           Control.Monad.Catch   (Exception, MonadThrow (..))
import           Control.Monad.Log     as Log
import           Control.Monad.Service (Service (run))
import           Data.Text             (Text)
import qualified Network.AMQP.Worker   as Worker hiding (publish)

import           Timely.Advances       (Advance (..), Advances)
import           Timely.Events         as Events
import           Timely.Transfers      (Transfers)
import qualified Timely.Transfers      as Transfers
import           Timely.Types.Guid     as Guid



queue :: Worker.Queue Advance
queue = Worker.topic Events.advancesActive "app.advances.send"






handler
  :: ( Service m Advances
     , Service m Transfers
     , MonadThrow m
     , MonadLog m
     )
  => Advance -> m ()
handler advance = do
  Log.context (Guid.toText $ advanceId advance)
  _ <- run $ Transfers.Credit advance
  Log.info "sent"
  pure ()




testQueue :: Worker.Queue Text
testQueue = Worker.topic Events.test "app.test"


data Bad = Bad Text
  deriving (Show, Eq)

instance Exception Bad


test
  :: ( Service m Advances
     , Service m Transfers
     , MonadThrow m
     , MonadLog m
     )
  => Text -> m ()
test asdf = do
  Log.context asdf
  Log.info "hello"
  throwM $ Bad "NOPE"
  pure ()
