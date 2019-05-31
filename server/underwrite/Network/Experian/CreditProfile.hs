{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Experian.CreditProfile where


import Control.Lens                            ((?~), (^.), (.~))
import Control.Monad.IO.Class                  (MonadIO, liftIO)
import Data.Aeson                              (FromJSON, ToJSON (toJSON))
import Data.Function                           ((&))
import Data.String.Conversions                 (cs)
import Data.Text                               (Text)
import GHC.Generics                            (Generic)
import Network.Experian.CreditProfile.Request  (Request)
import Network.Experian.CreditProfile.Response
import Network.Wreq                            as Wreq (asJSON, auth, defaults, oauth2Bearer, postWith, responseBody, header)
import System.FilePath                         ((</>))





-- https://uat-us-api.experian.com/oauth2/v1/token
-- https://uat-us-api.experian.com/consumerservices/credit-profile/v2/credit-report

type Endpoint = String

newtype AccessToken = AccessToken Text
  deriving (Show, Eq, FromJSON)


data Login = Login
  { username :: Text
  , password :: Text
  } deriving (Show, Generic)
instance ToJSON Login


data Credentials = Credentials
  { clientId :: Text
  , clientSecret :: Text
  , login :: Login
  } deriving (Show)


data AuthResponse = AuthResponse
  { access_token :: Text
  } deriving (Show, Generic)
instance FromJSON AuthResponse


authenticate :: MonadIO m => Endpoint -> Credentials -> m AccessToken
authenticate endpoint creds = do
  let url = endpoint </> "oauth2/v1/token"

  let opts = defaults & header "client_id" .~ [cs $ clientId creds]
                      & header "client_secret" .~ [cs $ clientSecret creds]

  res <- liftIO $ Wreq.postWith opts url (toJSON (login creds)) >>= asJSON
  pure $ AccessToken $ access_token $ res ^. responseBody


creditProfile :: MonadIO m => Endpoint -> AccessToken -> Request -> m Response
creditProfile endpoint (AccessToken tok) req = do
  let url = endpoint </> "consumerservices/credit-profile/v2/credit-report"

  let opts = defaults & auth ?~ (oauth2Bearer $ cs tok)

  res <- liftIO $ Wreq.postWith opts url (toJSON req) >>= asJSON
  pure $ res ^. responseBody
