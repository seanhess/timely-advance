{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Session where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Model.Guid       (Guid)
import           Data.Model.Types      (Phone)
import           Data.Model.Valid      (Valid)
import           GHC.Generics          (Generic)
import           Servant.Auth.Server   (FromJWT, ToJWT)
import           Timely.Accounts.Types (Account)



data Session = Session
    { phone     :: Valid Phone
    , accountId :: Maybe (Guid Account)
    , isAdmin   :: Bool
    } deriving (Generic, Show)

instance FromJSON Session
instance ToJSON Session
instance ToJWT Session
instance FromJWT Session



data Admin
