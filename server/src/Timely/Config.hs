{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Config where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Maybe              as Maybe
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Typeable           (Typeable)
import           Database.Selda          (MonadMask)
import           GHC.Generics            (Generic)
import           Network.Authy           (AuthyApiKey)
import           Network.Plaid.Types     (Client, Id (..), Public, Secret)
import           Servant.Client          (BaseUrl (..), Scheme (Https))
import qualified Servant.Client          as Servant
import           System.Envy             (DefConfig (..), FromEnv, Var (..))
import qualified System.Envy             as Envy
import           Timely.Auth             (Phone (..), phone)
import qualified Twilio                  as Twilio

data Env = Env
  { postgres          :: Text
  , amqp              :: Text
  , plaidBaseUrl      :: BaseUrl
  , plaidPublicKey    :: Id Public
  , plaidClientId     :: Id Client
  , plaidClientSecret :: Id Secret
  , authyBaseUrl      :: BaseUrl
  , authyApiKey       :: AuthyApiKey
  , twilioAccountId   :: Twilio.AccountSID
  , twilioAuthToken   :: Twilio.AuthToken
  , twilioFromPhone   :: Phone
  , endpoint          :: BaseUrl
  } deriving (Show, Eq, Generic)


instance DefConfig Env where
  defConfig = Env
    { postgres          = "postgresql://postgres@localhost:5432"
    , amqp              = "amqp://guest:guest@localhost:5672"
    , plaidBaseUrl      = BaseUrl Https "sandbox.plaid.com" 443 ""
    , plaidPublicKey    = Id "447ab26f3980c45b7202e2006dd9bf"
    , plaidClientId     = Id "5c1a663c5eca930011ff67ee"
    , plaidClientSecret = Id "db8bad5d68d41340cba767615c7aea"
    , authyApiKey       = "bmGKSWu6xZ8vGEhtfvsIBZKcxLarHs64"
    , authyBaseUrl      = BaseUrl Https "api.authy.com" 443 ""
    , twilioAccountId   = Twilio.AccountSID "ACea89d7047fbce75c97607b517303f27a"
    , twilioAuthToken   = Maybe.fromJust $ Twilio.parseAuthToken "01aadd9eee8a895d9f410b5e807334ee"
    , twilioFromPhone   = "5413940563"
    , endpoint          = BaseUrl Https "app.timelyadvance.com" 443 ""
    }

instance FromEnv Env

instance Var Phone where
  toVar (Phone x) = cs x
  fromVar x = phone $ cs x

instance Var Twilio.AccountSID where
  toVar x = cs $ Twilio.getSID x
  fromVar x = Twilio.parseSID $ cs x

instance Var Twilio.AuthToken where
  toVar = cs . Twilio.getAuthToken
  fromVar = Twilio.parseAuthToken . cs

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
    Left err  -> error err
    Right env -> pure env
