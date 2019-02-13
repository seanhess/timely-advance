{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
module Timely.Auth where

import           Control.Exception         (Exception, throw)
import           Control.Monad.Config      (MonadConfig (..), configs)
import           Control.Monad.Except      (MonadError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Service     (Service (..))
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Model.Types          (Phone, Valid(..))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Network.Authy             (AuthyApiKey)
import qualified Network.Authy             as Authy
import           Network.HTTP.Client       (Manager)
import           Network.HTTP.Types        (status401)
import           Servant
import           Servant.Auth.Server
import           Servant.Client            (BaseUrl, ClientEnv, ClientM, GenResponse (..), ServantError (..),
                                            mkClientEnv, runClientM)

-- Roles
-- 1. Generate SMS codes
-- 2. Verify SMS codes
-- 3. Provide some auth checking helpers to the rest of the system


-- TODO properly parse phones




newtype AuthCode = AuthCode Text
   deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance MimeUnrender PlainText AuthCode where
    mimeUnrender _ s = AuthCode <$> mimeUnrender (Proxy :: Proxy PlainText) s



data AuthService a where
    CodeGenerate :: Valid Phone -> AuthService ()
    CodeCheck    :: Valid Phone -> AuthCode -> AuthService Bool


instance (MonadIO m, MonadConfig AuthConfig m) => Service m AuthService where
    run (CodeGenerate p) = verifyStart p
    run (CodeCheck p c)  = verifyCheck p c



data AuthConfig = AuthConfig
    { manager :: Manager
    , baseUrl :: BaseUrl
    , apiKey  :: AuthyApiKey
    }



verifyStart :: (MonadIO m, MonadConfig AuthConfig m) => Valid Phone -> m ()
verifyStart (Valid p) = do
  key <- configs apiKey
  _ <- runAuthy (Authy.reqVerifyStart key p)
  pure ()


verifyCheck :: (MonadIO m, MonadConfig AuthConfig m) => Valid Phone -> AuthCode -> m Bool
verifyCheck (Valid p) (AuthCode c) = do
  key <- configs apiKey
  env <- clientEnv
  res <- liftIO $ runClientM (Authy.reqVerifyCheck key p c) env
  -- liftIO $ putStrLn "COMPLETED!"
  case res of
    Left (FailureResponse gres) ->
      if responseStatusCode gres == status401
         then pure False
         else throw $ AuthyError $ FailureResponse gres
    Left e -> throw $ AuthyError e
    Right _ -> pure True





data AuthyError = AuthyError ServantError
    deriving (Show, Eq)
instance Exception AuthyError


-- 404 on no pending verifications (500 error, should not happen)
-- 401 on not authorized
runAuthy :: (MonadIO m, MonadConfig AuthConfig m) => ClientM a -> m a
runAuthy req = do
  env <- clientEnv
  res <- liftIO $ runClientM req env
  case res of
    Left err -> throw $ AuthyError err
    Right a  -> pure a


clientEnv :: (MonadConfig AuthConfig m) => m ClientEnv
clientEnv = do
    cfg <- config
    pure $ mkClientEnv (manager cfg) (baseUrl cfg)




login :: (ToJWT session, MonadIO m, MonadError ServantErr m) => CookieSettings -> JWTSettings -> session -> value -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie ] value)
login cke jwt session value = do
  mApplyCookies <- liftIO $ acceptLogin cke jwt session
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> pure $ applyCookies value


logout :: Applicative m => CookieSettings -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logout cke =
  pure $ clearSession cke NoContent
