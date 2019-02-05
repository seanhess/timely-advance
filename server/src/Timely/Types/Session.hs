{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Session where

import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Servant.Auth.Server       (FromJWT, ToJWT)

import           Timely.AccountStore.Types (Account)
import           Timely.Auth               (Phone)
import           Timely.Types.Guid         (Guid)



data Session = Session
    { phone     :: Phone
    , accountId :: Maybe (Guid Account)
    , isAdmin   :: Bool
    } deriving (Generic, Show)

instance FromJSON Session
instance ToJSON Session
instance ToJWT Session
instance FromJWT Session



data Admin

