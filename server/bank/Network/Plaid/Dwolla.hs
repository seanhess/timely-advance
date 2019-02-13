{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Plaid.Dwolla where


import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Model.Id       (Id, Token)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types
import           Servant


-- Dwolla -------------------
-- # Create processor token
-- curl -X POST
-- https://sandbox.plaid.com/processor/dwolla/processor_token/create \
-- -H 'Content-Type: application/json'
-- -d '{
--   "client_id": "[Plaid Client ID],
--   "secret": "[Plaid secret]",
--   "access_token": "[Access token]",
--   "account_id": "[Account ID]",
-- }'


type Endpoint = "processor" :> "dwolla" :> "processor_token" :> "create"
        :> ReqBody '[JSON] Request
        :> Post    '[JSON] Response

data Dwolla


data Request = Request
    { client_id    :: Id Client
    , secret       :: Id Secret
    , access_token :: Token Access
    , account_id   :: Id Account
    } deriving (Generic, Show, Eq)

instance ToJSON Request


data Response = Response
    { processor_token :: Token Dwolla
    , request_id      :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON Response
