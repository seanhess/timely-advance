{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Plaid.Accounts where


import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types
import           Servant

-- * Accounts
--
-- Account balances
--
-- > curl -X POST https://sandbox.plaid.com/accounts/get \
-- > -H 'Content-Type: application/json' \
-- > -d '{
-- >   "client_id": String,
-- >   "secret": String,
-- >   "access_token": String
-- > }'

-- > http code 200
-- > {
-- >   "accounts": [{
-- >     "account_id": "vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D",
-- >     "balances": {
-- >       "available": 100,
-- >       "current": 110,
-- >       "limit": null,
-- >       "iso_currency_code": "USD",
-- >       "unofficial_currency_code": null,
-- >     },
-- >     "mask": "0000",
-- >     "name": "Plaid Checking",
-- >     "official_name": "Plaid Gold Checking",
-- >     "subtype": "checking",
-- >     "type": "depository"
-- >   }, {
-- >     "account_id": "6Myq63K1KDSe3lBwp7K1fnEbNGLV4nSxalVdW",
-- >     "balances": {
-- >       "available": null,
-- >       "current": 410,
-- >       "limit": 2000,
-- >       "iso_currency_code": "USD",
-- >       "unofficial_currency_code": null,
-- >     },
-- >     "mask": "3333",
-- >     "name": "Plaid Credit Card",
-- >     "official_name": "Plaid Diamond Credit Card",
-- >     "subtype": "credit card",
-- >     "type": "credit"
-- >   }],
-- >   "item": {Object},
-- >   "request_id": "m8MDnv9okwxFNBV"
-- > }


type Endpoint = "accounts" :> "get"
      :> ReqBody '[JSON] Request
      :> Post    '[JSON] Response


data Request = Request
    { client_id :: Id Client
    , secret :: Id Secret
    , access_token :: Token Access
    } deriving (Generic, Show, Eq)

instance ToJSON Request

data Response = Response
    { accounts :: [ Account ]
    , item :: Item
    , request_id :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON Response



