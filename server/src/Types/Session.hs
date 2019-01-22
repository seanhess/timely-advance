module Types.Session where

import Data.Aeson (ToJSON, FromJSON)
import Servant.Auth.Server (ToJWT, FromJWT)
import GHC.Generics (Generic)

import Types.Guid (Guid)
import AccountStore.Types (Account)
import Auth (Phone)

data Session = Session
    { phone :: Phone
    , accountId :: Maybe (Guid Account)
    } deriving (Generic, Show)

instance FromJSON Session
instance ToJSON Session
instance ToJWT Session
instance FromJWT Session
