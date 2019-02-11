{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Plaid.Identity where


import           Data.Aeson          as Aeson
import           Data.Text           as Text
import           Data.List           as List
import           GHC.Generics        (Generic)
import           Network.Plaid.Types
import           Servant


-- * Identity

-- curl -X POST https://sandbox.plaid.com/identity/get \
-- -H 'Content-Type: application/json' \
-- -d '{
--   "client_id": String,
--   "secret": String,
--   "access_token": String
-- }'

-- http code 200
-- {
--   "accounts": [{object}],
--   "identity": {
--     "addresses": [
--       {
--         "accounts": [
--           "Plaid Checking 0000",
--           "Plaid Saving 1111",
--           "Plaid CD 2222"
--         ],
--         "data": {
--           "city": "Malakoff",
--           "state": "NY",
--           "street": "2992 Cameron Road",
--           "zip": "14236"
--         },
--         "primary": true
--       },
--       {
--         "accounts": [
--           "Plaid Credit Card 3333"
--         ],
--         "data": {
--           "city": "San Matias",
--           "state": "CA",
--           "street": "2493 Leisure Lane",
--           "zip": "93405-2255"
--         },
--         "primary": false
--       }
--     ],
--     "emails": [
--       {
--         "data": "accountholder0@example.com",
--         "primary": true,
--         "type": "primary"
--       }
--     ],
--     "names": [
--       "Alberta Bobbeth Charleson"
--     ],
--     "phone_numbers": [{
--       "primary": true,
--       "type": "home",
--       "data": "4673956022"
--     }],
--   },
--   "item": {object},
--   "request_id": "m8MDnv9okwxFNBV"
-- }

type Endpoint = "identity" :> "get"
      :> ReqBody '[JSON] Request
      :> Post    '[JSON] Response



data Request = Request
    { client_id    :: Id Client
    , secret       :: Id Secret
    , access_token :: Token Access
    } deriving (Generic, Show, Eq)

instance ToJSON Request

data Response = Response
    { accounts   :: [ Account ]
    , item       :: Item
    , identity   :: Identity
    , request_id :: Id Request
    } deriving (Generic, Show, Eq)

instance FromJSON Response

data Identity = Identity
    { addresses     :: [AddressInfo]
    , names         :: [Name]
    , emails        :: [IdentityInfo]
    , phone_numbers :: [IdentityInfo]
    } deriving (Generic, Show, Eq)

instance FromJSON Identity
type Name = Text

data IdentityInfo = IdentityInfo
    { _data    :: Text
    , _primary :: Bool
    , _type    :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON IdentityInfo where
    parseJSON = Aeson.genericParseJSON prefixOptions

data AddressInfo = AddressInfo
    { _data     :: Address
    , _primary  :: Bool
    , _accounts :: [Text]
    } deriving (Generic, Show, Eq)

instance FromJSON AddressInfo where
    parseJSON = Aeson.genericParseJSON prefixOptions


prefixOptions :: Aeson.Options
prefixOptions = Aeson.defaultOptions { fieldLabelModifier = List.drop 1 }


data Address = Address
    { street :: Text
    , city   :: Text
    , state  :: Text
    , zip    :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON Address
