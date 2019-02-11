{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.Plaid.ExchangeToken where


import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types
import           Servant


-- * Authentication
--
-- Exchange Tokens
--
-- > curl -X POST https://sandbox.plaid.com/item/public_token/exchange \
-- >  -H 'Content-Type: application/json' \
-- >   -d '{
-- >     "client_id": String,
-- >     "secret": String,
-- >     "public_token":
-- > "public-sandbox-5c224a01-8314-4491-a06f-39e193d5cddc"
-- >   }'

-- > http code 200
-- > {
-- >   "access_token":
-- > "access-sandbox-de3ce8ef-33f8-452c-a685-8671031fc0f6",
-- >   "item_id": "M5eVJqLnv3tbzdngLDp9FL5OlDNxlNhlE55op",
-- >   "request_id": "Aim3b"
-- > }

type Endpoint = "item" :> "public_token" :> "exchange"
        :> ReqBody '[JSON] Request
        :> Post    '[JSON] Response


data Request = Request
    { client_id    :: Id Client
    , secret       :: Id Secret
    , public_token :: Token Public
    } deriving (Generic, Show, Eq)

instance ToJSON Request


data Response = Response
    { access_token :: Token Access
    , item_id      :: Id Item
    , request_id   :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON Response
