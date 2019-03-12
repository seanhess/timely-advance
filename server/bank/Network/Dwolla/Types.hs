{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Network.Dwolla.Types where


import           Data.Aeson              as Aeson
import           Data.Model.Id           (Id (..))
import           Data.Proxy              as Proxy
import           Data.String.Conversions (cs)
import           Data.Text               as Text
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)
import qualified Numeric
import           Servant                 (FromHttpApiData (..), ToHttpApiData (..))


newtype Resource a = Resource Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromHttpApiData)

data Static (s :: Symbol) = Static
  deriving (Show, Eq)

instance forall s. KnownSymbol s => ToJSON (Static s) where
  toJSON _ = Aeson.String $ cs $ symbolVal (Proxy :: Proxy s)

instance forall s. KnownSymbol s => ToHttpApiData (Static s) where
  toUrlPiece _ = cs $ symbolVal (Proxy :: Proxy s)

-- newtype Last4SSN = Last4SSN Text
--   deriving (Show, Eq, Generic, ToJSON)

-- last4SSN :: Text -> Last4SSN
-- last4SSN t = Last4SSN $ Text.takeEnd 4 t

-- newtype PhoneDigits = PhoneDigits Text
--   deriving (Show, Eq, Generic, ToJSON)

-- newtype Address = Address Text
--   deriving (Show, Eq, Generic)

-- instance ToJSON Address where
--   toJSON (Address t) = Aeson.String $ Text.take 50 t

newtype Amount = Amount Float
  deriving (Show, Eq)

instance ToJSON Amount where
  toJSON (Amount f) = Aeson.String $ cs $ Numeric.showFFloat (Just 2) f ""


data RelLink a = RelLink
  { href :: Resource a
  } deriving (Show, Eq, Generic)


instance ToJSON (RelLink a)
instance FromJSON (RelLink a)


resourceToId :: Resource a -> Maybe (Id a)
resourceToId (Resource a) =
  Just $ Id $ Text.takeWhileEnd ((/=) '/') a
