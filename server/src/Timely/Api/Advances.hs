{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Advances where

import           Control.Effects             (MonadEffects)
import           Control.Effects.Log         as Log
import           Control.Effects.Signal      (Throw, throwSignal)
import           Control.Effects.Worker      (Publish)
import qualified Control.Effects.Worker      as Worker
import           Control.Monad               (when)
import           Control.Monad.Service       (Service (..))
import           Data.Model.Guid             as Guid
import           Data.Model.Money            (Money)
import           Servant                     (ServantErr (..), err400)
import           Timely.AccountStore.Account (Accounts)
import qualified Timely.AccountStore.Account as Accounts
import           Timely.AccountStore.Types   (Account)
import           Timely.Advances             (Advance (..), Advances (..))
import qualified Timely.Advances             as Advances
import qualified Timely.Advances.Credit      as Credit
import           Timely.Api.Combinators      (notFound)
import           Timely.Api.Types            as Types (Amount (..))
import qualified Timely.Events               as Events


acceptAdvance
  :: ( Service m Advances
     , MonadEffects '[Throw ServantErr, Log, Publish, Accounts] m
     ) => Guid Account -> Guid Advance -> Amount -> m Advance
acceptAdvance a adv amt = do
  Log.context "acceptAdvance"
  Log.context (Guid.toText adv)
  Log.debug ("amount", amt)
  checkCredit a (Types.amount amt)
  run $ Advances.Activate a adv (Types.amount amt)
  advance <- run (Advances.Find a adv) >>= notFound
  Worker.publish Events.advancesActive advance
  pure advance


-- TODO tests
checkCredit
  :: ( MonadEffects '[Throw ServantErr, Accounts] m
     , Service m Advances
     ) => Guid Account -> Money -> m ()
checkCredit a amount = do
  account <- Accounts.find a >>= notFound
  advances <- run (Advances.FindActive a)

  when (not $ Credit.isEnough amount account advances) $ do
    throwSignal $ err400 { errBody = "Insufficient Credit" }
