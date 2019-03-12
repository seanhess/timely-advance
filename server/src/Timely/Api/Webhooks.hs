{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Webhooks
  ( plaid
  , Plaid
  ,) where


-- import Network.Plaid.Webhooks (Webhook(..))
import           Control.Effects             (MonadEffects)
import           Control.Effects.Log         (Log)
import qualified Control.Effects.Log         as Log
import           Control.Effects.Signal      (Throw, handleException, throwSignal)
import           Control.Effects.Worker      (Publish)
import qualified Control.Effects.Worker      as Worker
import           Data.Model.Guid             (Guid)
import           Data.Model.Id               (Id)
import           Data.String.Conversions     (cs)
import qualified Network.Plaid.Types         as Plaid
import qualified Network.Plaid.Webhooks      as Plaid
import           Servant                     (NoContent (..))
import           Timely.AccountStore.Account (Accounts)
import qualified Timely.AccountStore.Account as Accounts
import           Timely.AccountStore.Types   (Account)
import qualified Timely.Events               as Events

type Plaid = Plaid.Webhook


data Error
  = MissingBankId (Id Plaid.Item)
  deriving (Show)



-- handle signals here?
plaid :: MonadEffects '[Log, Accounts, Publish] m => Plaid.Webhook -> m NoContent
plaid hook = do
  Log.context "webhooks.plaid"
  handleException onError (handle hook)
  pure NoContent
  where
    handle (Plaid.WebhookTransactions info) = transactions info
    handle (Plaid.WebhookUnknown val)       = Log.error ("Unknown", val)

    onError :: MonadEffects '[Log] m => Error -> m ()
    onError err = Log.error err


transactions
  :: MonadEffects '[Log, Accounts, Throw Error, Publish] m
  => Plaid.Transactions -> m ()
transactions info = do
  -- it's going to come in twice. We want to ignore INITIAL_UPDATE only use HISTORICAL_UPDATE and DEFAULT_UPDATE
  Log.context "transactions"
  Log.info $ cs $ show $ Plaid.item_id info
  a <- findAccountByBankId (Plaid.item_id info)
  Log.info $ cs $ show a
  Worker.publish Events.transactionsNew a


findAccountByBankId
  :: (MonadEffects '[Accounts, Throw Error] m)
  => Id Plaid.Item -> m (Guid Account)
findAccountByBankId i = do
  ma <- Accounts.findByBankId i
  case ma of
    (Just a) -> pure a
    Nothing  -> throwSignal $ MissingBankId i
