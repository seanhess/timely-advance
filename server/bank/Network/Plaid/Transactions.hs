{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Plaid.Transactions where


import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Model.Id       (Id (..), Token (..))
import           Data.Time.Calendar  (Day)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types
import           Servant

-- * Transactions
--
-- Get lists of transactions
--
-- > curl -X POST https://sandbox.plaid.com/transactions/get \
-- > -H 'Content-Type: application/json' \
-- > -d '{
-- >   "client_id": String,
-- >   "secret": String,
-- >   "access_token": String,
-- >   "start_date": "2018-01-01",
-- >   "end_date": "2018-02-01",
-- >   "options": {
-- >     "count": 250,
-- >     "offset": 100
-- >   }
-- > }'

-- > http code 200
-- > {
-- >  "accounts": [{object}],
-- >  "transactions": [{
-- >     "account_id": "vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d",
-- >     "amount": 2307.21,
-- >     "iso_currency_code": "USD",
-- >     "unofficial_currency_code": null,
-- >     "category": [
-- >       "Shops",
-- >       "Computers and Electronics"
-- >     ],
-- >     "category_id": "19013000",
-- >     "date": "2017-01-29",
-- >     "location": {
-- >      "address": "300 Post St",
-- >      "city": "San Francisco",
-- >      "state": "CA",
-- >      "zip": "94108",
-- >      "lat": null,
-- >      "lon": null
-- >     },
-- >     "name": "Apple Store",
-- >     "payment_meta": Object,
-- >     "pending": false,
-- >     "pending_transaction_id": null,
-- >     "account_owner": null,
-- >     "transaction_id": "lPNjeW1nR6CDn5okmGQ6hEpMo4lLNoSrzqDje",
-- >     "transaction_type": "place"
-- >    }, {
-- >     "account_id": "XA96y1wW3xS7wKyEdbRzFkpZov6x1ohxMXwep",
-- >     "amount": 78.5,
-- >     "iso_currency_code": "USD",
-- >     "unofficial_currency_code": null,
-- >     "category": [
-- >       "Food and Drink",
-- >       "Restaurants"
-- >     ],
-- >     "category_id": "13005000",
-- >     "date": "2017-01-29",
-- >     "location": {
-- >       "address": "262 W 15th St",
-- >       "city": "New York",
-- >       "state": "NY",
-- >       "zip": "10011",
-- >       "lat": 40.740352,
-- >       "lon": -74.001761
-- >     },
-- >     "name": "Golden Crepes",
-- >     "payment_meta": Object,
-- >     "pending": false,
-- >     "pending_transaction_id": null,
-- >     "account_owner": null,
-- >     "transaction_id": "4WPD9vV5A1cogJwyQ5kVFB3vPEmpXPS3qvjXQ",
-- >     "transaction_type": "place"
-- >   }],
-- >   "item": {Object},
-- >   "total_transactions": Number,
-- >   "request_id": "45QSn"
-- > }


type Endpoint = "transactions" :> "get"
      :> ReqBody '[JSON] Request
      :> Post    '[JSON] Response


data Options = Options
    { start_date  :: Day
    , end_date    :: Day
    , count       :: Int
    , offset      :: Int
    }


data Request = Request
    { client_id    :: Id Client
    , secret       :: Id Secret
    , access_token :: Token Access
    , start_date   :: Day
    , end_date     :: Day
    , options      :: ListRequestOptions
    } deriving (Generic, Show, Eq)

instance ToJSON Request


data ListRequestOptions = ListRequestOptions
    { count       :: Int
    , offset      :: Int
    , account_ids :: [ Id Account ]
    } deriving (Generic, Show, Eq)

instance ToJSON ListRequestOptions


data Response = Response
    { accounts           :: [ Account ]
    , transactions       :: [ Transaction ]
    , item               :: Item
    , total_transactions :: Int
    -- , request_id :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON Response



