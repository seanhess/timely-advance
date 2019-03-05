{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Sessions where


import           Control.Effects             (MonadEffects)
import           Control.Effects.Signal      (Throw, throwSignal)
import           Control.Monad.Config
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Service       (Service (..))
import           Crypto.JOSE.JWK             (JWK)
import           Data.ByteString             (ByteString)
import           Data.Model.Guid             as Guid
import           Data.Model.Id               (Token (..))
import           Data.Model.Types            (Phone)
import           Data.Model.Valid            as Valid
import           Servant                     (Header, Headers, NoContent (..), ServantErr, err401)
import           Servant.Auth.Server         (AuthResult (..), CookieSettings (..), IsSecure (..), JWTSettings,
                                              SetCookie, ThrowAll (..), defaultCookieSettings, defaultJWTSettings)
import qualified Servant.Auth.Server         as Servant

import qualified Timely.AccountStore.Account as Account
import Timely.AccountStore.Account (Accounts)
import           Timely.AccountStore.Types   (Account)
import           Timely.Auth                 (AuthCode, AuthConfig)
import qualified Timely.Auth                 as Auth
import           Timely.Types.Session        (Admin, Session (..))


type SetSession a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a



generateCode :: Service m Auth.AuthService => Valid Phone -> m NoContent
generateCode p = do
  run $ Auth.CodeGenerate p
  pure NoContent


-- TODO check to see if there's an account and set the account id
authenticate
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr, Accounts] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     , MonadConfig AuthConfig m
     ) => Valid Phone -> AuthCode -> m (SetSession Session)
authenticate p c = do
  res <- run $ Auth.CodeCheck p c
  if not res
     then throwSignal err401
     else session p


authAdmin
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     , MonadConfig (Token Admin) m
     ) => Token Admin -> m (SetSession Session)
authAdmin check = do
  liftIO $ print ("HI", check)
  good <- config
  let session = Session (Valid "8012223333") Nothing True
  if check == good
     then setSession session session
     else throwSignal err401


session
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr, Accounts] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Valid Phone -> m (SetSession Session)
session p = do
    -- they've already successfully validated the code. They're in!
    ma <- Account.findByPhone p
    let s = Session p ma False
    setSession s s


setSession
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Session -> value -> m (SetSession value)
setSession s value = do
    cke <- config
    jwt <- config
    mrespond <- Auth.login cke jwt s
    case mrespond of
      Nothing      -> throwSignal err401
      Just respond -> pure $ respond value



checkSession
  :: ( MonadEffects '[Throw ServantErr] m
     ) => AuthResult Session -> m (Session)
checkSession (Authenticated s) = pure s
checkSession _ =
  throwSignal err401



logout :: (Monad m, MonadConfig CookieSettings m) => m (SetSession NoContent)
logout = do
  cke <- config
  Auth.logout cke


jwtSettings jwk = defaultJWTSettings jwk
cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing }


generateKey :: MonadIO m => m JWK
generateKey = liftIO $ Servant.generateKey


secretKey :: ByteString -> JWK
secretKey = Servant.fromSecret



protectPhone :: ThrowAll api => (Valid Phone -> api) -> AuthResult Session -> api
protectPhone api (Authenticated (Session p _ _)) = api p
protectPhone _ _                                 = throwAll err401


protectAccount :: ThrowAll api => ((Guid Account) -> api) -> AuthResult Session -> Guid Account -> api
protectAccount api (Authenticated (Session _ (Just a) _)) a2
  | a == a2 = api a
  | otherwise = throwAll err401
protectAccount api (Authenticated (Session _ _ True)) a2 = api a2
protectAccount _ _ _ = throwAll err401
