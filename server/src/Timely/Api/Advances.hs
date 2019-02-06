{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Advances where

import           Control.Monad               (when)
import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Service       (Service (..))
import           Network.AMQP.Worker.Monad   (MonadWorker)
import qualified Network.AMQP.Worker.Monad   as Worker
import           Servant                     (ServantErr (..), err400)

import           Timely.AccountStore.Account (AccountStore)
import qualified Timely.AccountStore.Account as Accounts
import           Timely.AccountStore.Types   (Account)
import           Timely.Advances             (Advance (..), Advances (..))
import qualified Timely.Advances             as Advances
import qualified Timely.Advances.Credit      as Credit
import           Timely.Api.Combinators      (notFound)
import           Timely.Api.Types            as Types (Amount (..))
import qualified Timely.Events               as Events
import           Timely.Types.Guid           (Guid)
import           Timely.Types.Money          (Money)


acceptAdvance
  :: ( MonadWorker m
     , Service m Advances
     , Service m AccountStore
     , MonadError ServantErr m
     ) => Guid Account -> Guid Advance -> Amount -> m Advance
acceptAdvance a adv amt = do
  checkCredit a (Types.amount amt)
  run $ Advances.Activate a adv (Types.amount amt)
  advance <- run (Advances.Find a adv) >>= notFound
  Worker.publish Events.advancesActive advance
  pure advance


-- TODO tests
checkCredit
  :: ( MonadError ServantErr m
     , Service m AccountStore
     , Service m Advances
     ) => Guid Account -> Money -> m ()
checkCredit a amount = do
  account <- run (Accounts.Find a) >>= notFound
  advances <- run (Advances.FindActive a)

  when (not $ Credit.isEnough amount account advances) $ do
    throwError $ err400 { errBody = "Insufficient Credit" }


