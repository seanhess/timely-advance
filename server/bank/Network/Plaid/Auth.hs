{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Plaid.Auth where


import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types
import           Servant

type Endpoint = "auth" :> "get"
        :> ReqBody '[JSON] Request
        :> Post    '[JSON] Response




data Request = Request
    { client_id    :: Id Client
    , secret       :: Id Secret
    , access_token :: Token Access
    } deriving (Generic, Show, Eq)

instance ToJSON Request


data Response = Response
    { accounts :: [ Account ]
    , numbers  :: AuthNumbers
    , request_id :: Id Request
    , item     :: Item
    } deriving (Generic, Show, Eq)

instance FromJSON Response



data AuthNumbers = AuthNumbers
    { ach :: [ Ach ]
    , eft :: [ Eft ]
    } deriving (Generic, Show, Eq)

instance FromJSON AuthNumbers


data Ach = Ach
    { account_id   :: Id Account
    , account      :: Number Account
    , routing      :: Number Routing
    , wire_routing :: Maybe (Number WireRouting)
    } deriving (Generic, Show, Eq)

instance FromJSON Ach

data Eft = Eft
    { account_id  :: Id Account
    , account     :: Number Account
    , institution :: Number Institution
    , branch      :: Number Branch
    } deriving (Generic, Show, Eq)

instance FromJSON Eft




-- * Auth
--
-- Get ACH numbers
--
-- > curl -X POST https://sandbox.plaid.com/auth/get \
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
-- >   }],
-- >   "numbers": {
-- >      "ach": [{
-- >       "account": "9900009606",
-- >       "account_id": "vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D",
-- >       "routing": "011401533",
-- >       "wire_routing": "021000021"
-- >      }],
-- >      "eft": []
-- >   },
-- >   "item": {Object},
-- >   "request_id": "m8MDnv9okwxFNBV"
-- > }

