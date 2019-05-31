{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Experian.CreditProfile
  ( Endpoint
  , AccessToken(..)
  , Login(..)
  , Credentials(..)
  , AuthError(..)
  , authenticate
  , load
  , checkUnauthorized
  ) where


import Control.Lens                            ((?~), (^.), (.~))
import Control.Monad.IO.Class                  (MonadIO, liftIO)
import Control.Exception (throwIO, catch, try, SomeException, Exception)
import Data.Aeson                              (FromJSON, ToJSON (toJSON), Value)
import Data.Function                           ((&))
import Data.ByteString (ByteString)
import Data.String.Conversions                 (cs)
import Data.Text                               (Text)
import GHC.Generics                            (Generic)
import Network.Experian.CreditProfile.Request  (Request)
import Network.Wreq                            as Wreq (asJSON, auth, defaults, oauth2Bearer, postWith, responseBody, header, responseStatus)
import Network.HTTP.Client (HttpExceptionContent(..), HttpException(..))
import System.FilePath                         ((</>))
import Network.HTTP.Types.Status (status401)



-- TODO - I need a way to log the full response, like with the other one. So parse as Value and parse again later, no?


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


-- TODO return something different when we get an authentication failure 
-- rather than just blowing up.
-- 401 access token is invalid


data AuthError = Unauthorized ByteString
  deriving (Show)
instance Exception AuthError



load :: MonadIO m => Endpoint -> AccessToken -> Request -> m (Either AuthError Value)
load endpoint tok req = do
  liftIO $ try (load' endpoint tok req `catch` checkUnauthorized)


load' :: Endpoint -> AccessToken -> Request -> IO Value
load' endpoint (AccessToken tok) req = do
  let url = endpoint </> "consumerservices/credit-profile/v2/credit-report"

  let opts = defaults & auth ?~ (oauth2Bearer $ cs tok)

  res <- Wreq.postWith opts url (toJSON req) >>= asJSON
  pure $ res ^. responseBody


checkUnauthorized :: HttpException -> IO Value
checkUnauthorized e@(HttpExceptionRequest _ (StatusCodeException r b)) =
  if r ^. responseStatus == status401
    then throwIO $ Unauthorized b
    else throwIO e
checkUnauthorized e = throwIO e
