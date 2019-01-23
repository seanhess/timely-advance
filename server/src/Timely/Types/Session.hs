module Timely.Types.Session where

import Data.Aeson (ToJSON, FromJSON)
import Servant.Auth.Server (ToJWT, FromJWT)
import GHC.Generics (Generic)

import Timely.Types.Guid (Guid)
import Timely.AccountStore.Types (Account)
import Timely.Auth (Phone)

data Session = Session
    { phone :: Phone
    , accountId :: Maybe (Guid Account)
    } deriving (Generic, Show)

instance FromJSON Session
instance ToJSON Session
instance ToJWT Session
instance FromJWT Session
