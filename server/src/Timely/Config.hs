{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Timely.Config
  ( Env(..)
  , loadEnv
  , version
  ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.ByteString         (ByteString)
import qualified Data.Maybe              as Maybe
import           Data.Model.Id           (Id (..), Token (..))
import           Data.Model.Types        (Phone)
import           Data.Model.Valid        as Valid
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Typeable           (Typeable)
import           Data.Version            (showVersion)
import           Database.Selda          (MonadMask)
import           GHC.Generics            (Generic)
import           Network.Authy           (AuthyApiKey)
import           Network.Dwolla          (FundingSource)
import qualified Network.Dwolla          as Dwolla
import           Network.Plaid.Types     (Client, Public)
import qualified Network.Plaid.Types     as Plaid
import qualified Paths_timely            as Paths
import           Servant.Client          (BaseUrl (..), Scheme (..))
import qualified Servant.Client          as Servant
import           System.Envy             (DefConfig (..), FromEnv, Var (..))
import qualified System.Envy             as Envy
import           Text.Show.Pretty        (PrettyVal (..))
import           Timely.Types.Config     (PlaidProducts (..))
import           Timely.Types.Session    (Admin)
import qualified Twilio                  as Twilio


version :: String
version = showVersion Paths.version


data Env = Env
  { appEndpoint         :: BaseUrl
  , postgres            :: Text
  , amqp                :: Text
  , port                :: Int
  , serveDir            :: FilePath
  , plaidBaseUrl        :: BaseUrl
  , plaidPublicKey      :: Id Public
  , plaidClientId       :: Id Client
  , plaidClientSecret   :: Id Plaid.Secret
  , plaidEnv            :: Text
  , plaidProducts       :: PlaidProducts
  , plaidWebhook        :: Text
  , authyBaseUrl        :: BaseUrl
  , authyApiKey         :: AuthyApiKey
  , twilioAccountId     :: Twilio.AccountSID
  , twilioAuthToken     :: Twilio.AuthToken
  , twilioFromPhone     :: Valid Phone
  , sessionSecret       :: ByteString
  , adminPassphrase     :: Token Admin
  , dwollaBaseUrl       :: BaseUrl
  , dwollaAuthBaseUrl   :: BaseUrl
  , dwollaClientId      :: Dwolla.Id Dwolla.Client
  , dwollaSecret        :: Dwolla.Id Dwolla.Secret
  , dwollaFundingSource :: Dwolla.Id FundingSource
  } deriving (Show, Eq, Generic)


instance DefConfig Env where
  defConfig = Env
    { appEndpoint       = BaseUrl Http "localhost" 8000 ""
    , postgres          = "postgresql://postgres@localhost:5432"
    , amqp              = "amqp://guest:guest@localhost:5672"
    , serveDir          = "../web/build"
    , port              = 3001
    -- , plaidBaseUrl      = BaseUrl Https "development.plaid.com" 443 ""
    -- , plaidClientSecret = Id "ce8f112af86209ce870a6d01c0af76"
    -- , plaidEnv          = "development"
    , plaidBaseUrl      = BaseUrl Https "sandbox.plaid.com" 443 ""
    , plaidClientSecret = Id "db8bad5d68d41340cba767615c7aea"
    , plaidEnv          = "sandbox"
    , plaidPublicKey    = Id "447ab26f3980c45b7202e2006dd9bf"
    , plaidClientId     = Id "5c1a663c5eca930011ff67ee"
    , plaidProducts     = PlaidProducts [ "transactions", "auth" ]
    , plaidWebhook      = "https://enmbc6z0zjcvp.x.pipedream.net/"
    , authyApiKey       = "bmGKSWu6xZ8vGEhtfvsIBZKcxLarHs64"
    , authyBaseUrl      = BaseUrl Https "api.authy.com" 443 ""
    , twilioAccountId   = Twilio.AccountSID "ACea89d7047fbce75c97607b517303f27a"
    , twilioAuthToken   = Maybe.fromJust $ Twilio.parseAuthToken "01aadd9eee8a895d9f410b5e807334ee"
    , twilioFromPhone   = Valid "5413940563"
    , sessionSecret     = "cQfTjWnZr4u7x!A%D*G-KaNdRgUkXp2s"
    , adminPassphrase   = Token "rapidly scotland horses stuff"
    , dwollaBaseUrl     = BaseUrl Https "api-sandbox.dwolla.com" 443 ""
    , dwollaAuthBaseUrl = BaseUrl Https "accounts-sandbox.dwolla.com" 443 ""
    , dwollaClientId    = Id "6Oq3hTqVLFh5CRSINp4hI3xsTVygUfA7lDzY8XPSFjzkN6AXUE"
    , dwollaSecret      = Id "fuDJGJSB7A52onADZro2IeKtL4VHG4UfL8fWht00JdVkf5o1p0"
    , dwollaFundingSource = Id "7c584617-958b-48d2-8e41-c88ec415694d"
    }

instance FromEnv Env


instance PrettyVal Env

instance PrettyVal (Valid a) where
  prettyVal (Valid a) = prettyVal a

instance PrettyVal (Token a) where
  prettyVal (Token a) = prettyVal a

instance PrettyVal (Id a) where
  prettyVal (Id a) = prettyVal a

instance PrettyVal BaseUrl where
  prettyVal url = prettyVal $ Servant.showBaseUrl url

instance PrettyVal PlaidProducts where
  prettyVal (PlaidProducts ps) = prettyVal ps

instance PrettyVal Twilio.AccountSID where
  prettyVal x = prettyVal $ Twilio.getSID x

instance PrettyVal Twilio.AuthToken where
  prettyVal x = prettyVal $ Twilio.getAuthToken x

instance PrettyVal ByteString where
  prettyVal x = prettyVal (cs x :: Text)




------------------------------------------------


instance Var PlaidProducts where
  toVar (PlaidProducts ps) = cs $ Text.unwords ps
  fromVar t = Just $ PlaidProducts $ Text.words $ cs t

instance Typeable a => Var (Token a) where
  toVar (Token x) = cs x
  fromVar x = Just $ Token $ cs x

instance (Typeable a, Validate a) => Var (Valid a) where
  toVar (Valid x) = cs x
  fromVar x = validate $ cs x

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
