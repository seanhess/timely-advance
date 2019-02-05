{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AdvanceSend where

import           Control.Monad.Catch     (MonadThrow (..))
import           Control.Monad.Logger    as Log (MonadLogger, logInfoNS)
import           Control.Monad.Service   (Service)
import           Data.String.Conversions (cs)
import qualified Network.AMQP.Worker     as Worker hiding (publish)

import           Timely.Advances         (Advance (..), Advances)
import           Timely.Events           as Events



queue :: Worker.Queue Advance
queue = Worker.topic Events.advancesActive "app.advances.send"





handler
  :: ( Service m Advances
     , MonadThrow m
     , MonadLogger m
     )
  => Advance -> m ()
handler advance = do
  -- TODO save transfer object. Save init. Update saved.
  -- TODO transfer service
  logInfoNS "app.transfers.send" (cs $ show advance)


