{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Timely.Api.Advances where

import           Network.AMQP.Worker.Monad (MonadWorker)
import           Control.Monad.Service (Service(..))
import           Control.Monad.Except                 (MonadError)
import           Servant                              (ServantErr)

import qualified Timely.Advances as Advances
import Timely.Advances (Advances(..), Advance(..))
import Timely.Types.Guid (Guid)
import Timely.AccountStore.Types (Account)
import Timely.Api.Types as Types (Amount(..))
import Timely.Api.Combinators (notFound)


acceptAdvance
  :: ( MonadWorker m
     , Service m Advances
     , MonadError ServantErr m
     ) => Guid Account -> Guid Advance -> Amount -> m Advance
acceptAdvance a adv amt = do
  run $ Advances.Activate a adv (Types.amount amt)
  ma <- run $ Advances.Find a adv
  notFound ma


