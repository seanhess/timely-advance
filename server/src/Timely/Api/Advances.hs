{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds   #-}
module Timely.Api.Advances where

import           Control.Monad               (when)
import           Control.Monad.Except        (MonadError (..))
import           Control.Monad.Service       (Service (..))
import           Network.AMQP.Worker.Monad   (MonadWorker)
import           Servant                     (ServantErr (..), err400)

import           Timely.AccountStore.Account (AccountStore)
import qualified Timely.AccountStore.Account as Accounts
import           Timely.AccountStore.Types   (Account (credit))
import           Timely.Advances             (Advance (..), Advances (..))
import qualified Timely.Advances             as Advances
import           Timely.Api.Combinators      (notFound)
import           Timely.Api.Types            as Types (Amount (..))
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
  ma <- run $ Advances.Find a adv
  notFound ma


checkCredit
  :: ( MonadError ServantErr m
     , Service m AccountStore
     ) => Guid Account -> Money -> m ()
checkCredit a amount = do
  account <- run (Accounts.Find a) >>= notFound
  when (credit account < amount) $ do
    throwError $ err400 { errBody = "Insufficient Credit" }
