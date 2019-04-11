{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Advances where

import           Control.Effects        (MonadEffects)
import           Control.Effects.Log    as Log
import           Control.Effects.Signal (Throw, throwSignal)
import           Control.Effects.Worker (Publish)
import qualified Control.Effects.Worker as Worker
import           Control.Monad          (when)
import           Data.Model.Guid        as Guid
import           Data.Model.Money       (Money)
import           Servant                (ServantErr (..), err400)
import           Timely.Accounts        (Account, Accounts)
import qualified Timely.Accounts        as Accounts
import           Timely.Advances        (Advance (..), Advances (..))
import qualified Timely.Advances        as Advances
import qualified Timely.Advances.Credit as Credit
import           Timely.Api.Combinators (notFound)
import           Timely.Api.Types       as Types (Amount (..))
import qualified Timely.Events          as Events


data AdvanceError = InsufficientCredit

acceptAdvance
  :: ( MonadEffects '[Throw ServantErr, Log, Publish, Accounts, Advances] m
     ) => Guid Account -> Guid Advance -> Amount -> m Advance
acceptAdvance a adv amt = do
  Log.context "acceptAdvance"
  Log.context (Guid.toText adv)
  Log.debug ("amount", amt)
  checkCredit a (Types.amount amt)
  Advances.activate a adv (Types.amount amt)
  advance <- Advances.find a adv >>= notFound
  Worker.publish Events.advancesActive advance
  pure advance


-- TODO tests
checkCredit
  :: ( MonadEffects '[Throw ServantErr, Accounts, Advances] m
     ) => Guid Account -> Money -> m ()
checkCredit a amount = do
  account <- Accounts.find a >>= notFound
  advances <- Advances.findActive a

  when (not $ Credit.isEnough amount account advances) $ do
    throwSignal $ err400 { errBody = "Insufficient Credit" }
