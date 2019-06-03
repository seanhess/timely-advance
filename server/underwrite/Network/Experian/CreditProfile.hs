{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Network.Experian.CreditProfile
  ( Endpoint
  , AccessToken(..)
  , Credentials(..)
  , AuthError(..)
  , authenticate
  , load
  , checkUnauthorized
  ) where


import Control.Exception                      (Exception, catch, throwIO)
import Control.Lens                           ((.~), (?~), (^.))
import Control.Monad.IO.Class                 (MonadIO, liftIO)
import Data.Aeson                             as Aeson (FromJSON, ToJSON (toJSON), Value, object, (.=))
import Data.ByteString                        (ByteString)
import Data.Function                          ((&))
import Data.String.Conversions                (cs)
import Data.Text                              (Text)
import GHC.Generics                           (Generic)
import Network.Experian.CreditProfile.Request (Request)
import Network.HTTP.Client                    (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types.Status              (status401)
import Network.Wreq                           as Wreq (asJSON, auth, defaults, header, oauth2Bearer, postWith, responseBody, responseStatus)
import System.FilePath                        ((</>))



-- TODO - I need a way to log the full response, like with the other one. So parse as Value and parse again later, no?


-- https://uat-us-api.experian.com/oauth2/v1/token
-- https://uat-us-api.experian.com/consumerservices/credit-profile/v2/credit-report

type Endpoint = String

newtype AccessToken = AccessToken Text
  deriving (Show, Eq, FromJSON)


data Credentials = Credentials
  { clientId     :: Text
  , clientSecret :: Text
  , username     :: Text
  , password     :: Text
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
  let body = object ["username" .= username creds, "password" .= password creds]

  res <- liftIO $ Wreq.postWith opts url body >>= asJSON
  let tok = AccessToken $ access_token $ res ^. responseBody
  pure tok


data AuthError = Unauthorized ByteString
  deriving (Show)
instance Exception AuthError


load :: MonadIO m => Endpoint -> AccessToken -> Request -> m Value
load endpoint tok req = do
  liftIO $ load' endpoint tok req `catch` checkUnauthorized


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
