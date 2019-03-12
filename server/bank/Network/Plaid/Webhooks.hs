{-# LANGUAGE DeriveGeneric #-}
module Network.Plaid.Webhooks where

import Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON(..), Value)
import           Data.Model.Id       (Id)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types (Item)


-- {
--   "error": null,
--   "item_id": "qV6V1GPZNQSyDa8EBnJgTKzXjZd9pLCdvneAx",
--   "new_transactions": 16,
--   "webhook_code": "INITIAL_UPDATE",
--   "webhook_type": "TRANSACTIONS"
-- }


data Webhook
  = WebhookTransactions Transactions
  | WebhookUnknown Value

instance FromJSON Webhook where
  parseJSON val =
        WebhookTransactions <$> parseJSON val
    <|> pure (WebhookUnknown val)


-- https://plaid.com/docs/#transactions-webhooks
data Transactions = Transactions
  { item_id      :: Id Item
  , webhook_code :: TransactionsCode
  , new_transactions :: Int
  } deriving (Generic)

instance FromJSON Transactions

data TransactionsCode
  = INITIAL_UPDATE
  | HISTORICAL_UPDATE
  | DEFAULT_UPDATE
  -- | TRANSACTIONS_REMOVED
  deriving (Generic)

instance FromJSON TransactionsCode

-- data Type
--   = TRANSACTIONS
--   deriving (Generic)

-- instance FromJSON Type
