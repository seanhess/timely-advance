{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Webhooks
  ( plaid
  , Plaid
  ,) where


import           Control.Effects             (MonadEffects)
import           Control.Effects.Log         (Log)
import qualified Control.Effects.Log         as Log
import           Control.Effects.Worker      (Publish)
import qualified Control.Effects.Worker      as Worker
import qualified Data.Aeson                  as Aeson
import           Data.String.Conversions     (cs)
import qualified Network.Plaid.Webhooks      as Plaid
import           Servant                     (NoContent (..))
import           Timely.Accounts             (Accounts)
import qualified Timely.Accounts             as Accounts
import           Timely.Accounts.Application (Applications)
import qualified Timely.Accounts.Application as Applications
import qualified Timely.Events               as Events

type Plaid = Plaid.Webhook


-- data Error
--   = MissingBankId (Id Plaid.Item)
--   deriving (Show)



-- handle signals here?
plaid :: MonadEffects '[Log, Applications, Accounts, Publish] m => Plaid.Webhook -> m NoContent
plaid hook = do
  Log.context "webhooks.plaid"
  -- handleException onError (handle hook)
  handle hook
  pure NoContent
  where
    handle (Plaid.WebhookTransactions info) = transactions info
    handle (Plaid.WebhookUnknown val)       = Log.error $ "Unknown: " <> cs (Aeson.encode val)

    -- onError :: MonadEffects '[Log] m => Error -> m ()
    -- onError err = Log.error err


transactions
  :: MonadEffects '[Log, Applications, Accounts, Publish] m
  => Plaid.Transactions -> m ()
transactions info = do
  let itemId = Plaid.item_id info
  let total  = Plaid.new_transactions info
  Log.context "transactions"
  Log.context $ cs $ show itemId

  case Plaid.webhook_code info of
    Plaid.INITIAL_UPDATE ->
      pure ()

    Plaid.HISTORICAL_UPDATE -> do
      Log.info "historical_update"
      Applications.saveTransactions itemId total

    Plaid.DEFAULT_UPDATE -> do
      Log.info "default_update"
      accounts <- Accounts.findByBankId itemId
      mapM_ (Worker.publish Events.transactionsUpdate) accounts
