{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import Database.Selda (MonadMask(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Plaid.Types (Id(..), Client, Secret, Public)
import System.Envy (FromEnv, DefConfig(..), Var(..))
import Servant.Client (BaseUrl(..), Scheme(Https))
import qualified Servant.Client as Servant
import qualified System.Envy as Envy

data Env = Env
    { postgres :: Text
    , amqp :: Text
    , plaidBaseUrl :: BaseUrl
    , plaidPublicKey :: Id Public
    , plaidClientId :: Id Client
    , plaidClientSecret :: Id Secret
    } deriving (Show, Eq, Generic)


instance DefConfig Env where
    defConfig = Env
      { postgres = "postgresql://postgres@localhost:5432"
      , amqp = "amqp://guest:guest@localhost:5672"
      , plaidBaseUrl = BaseUrl Https "sandbox.plaid.com" 443 ""
      , plaidPublicKey = Id "447ab26f3980c45b7202e2006dd9bf"
      , plaidClientId = Id "5c1a663c5eca930011ff67ee"
      , plaidClientSecret = Id "db8bad5d68d41340cba767615c7aea"
      }

instance FromEnv Env

instance Var BaseUrl where
    toVar = Servant.showBaseUrl
    fromVar = Servant.parseBaseUrl

instance Typeable t => Var (Id t) where
    toVar (Id t) = toVar t
    fromVar s = Id <$> fromVar s


loadEnv :: (MonadIO m, MonadMask m) => m Env
loadEnv = do
    eenv <- liftIO $ Envy.decodeEnv
    case eenv of
      Left err -> error err
      Right env -> pure env
