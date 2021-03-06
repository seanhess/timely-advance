{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Sessions where


import           Control.Effects               (MonadEffects)
import           Control.Effects.Log           (Log)
import qualified Control.Effects.Log           as Log
import           Control.Effects.Signal        (Throw, throwSignal)
import           Control.Monad.Config
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Crypto.JOSE.JWK               (JWK)
import           Data.ByteString               (ByteString)
import           Data.Model.Guid               as Guid
import           Data.Model.Id                 (Token (..))
import           Data.Model.Types              (Phone)
import           Data.Model.Valid              as Valid
import           Servant                       (Header, Headers, NoContent (..), ServantErr, err401)
import           Servant.Auth.Server           (AuthResult (..), CookieSettings (..), IsSecure (..), JWTSettings, SetCookie, ThrowAll (..), defaultCookieSettings, defaultJWTSettings)
import qualified Servant.Auth.Server           as Servant

import           Timely.Accounts               (Account, Accounts)
import qualified Timely.Accounts               as Accounts
import qualified Timely.Accounts.Types.Account as Account
import           Timely.Auth                   (Auth, AuthCode (..))
import qualified Timely.Auth                   as Auth
import           Timely.Types.Session          (Admin, Session (..))


type SetSession a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a



generateCode :: MonadEffects '[Auth] m => Valid Phone -> m NoContent
generateCode p = do
  Auth.codeGenerate p
  pure NoContent


authenticate
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr, Accounts, Auth, Log] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Valid Phone -> AuthCode -> m (SetSession Session)
authenticate p c = do
  Log.context "Authenticate"
  liftIO $ print ("Authenticate", p, c)

  res <- Auth.codeCheck p c
  if res
    then session p
    else throwSignal err401




authAdmin
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     , MonadConfig (Token Admin) m
     ) => Token Admin -> m (SetSession Session)
authAdmin check = do
  good <- config
  let session = Session (Valid "8012223333") Nothing True
  if check == good
     then setSession session session
     else throwSignal err401


session
  :: ( MonadIO m
     , MonadEffects '[Throw ServantErr, Accounts, Log] m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Valid Phone -> m (SetSession Session)
session p = do
    -- they've already successfully validated the code. They're in!
    ma <- Accounts.findByPhone p
    let mai = Account.accountId <$> ma
    let s = Session p mai False
    Log.debug ("Session", mai)
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
hour = 60*60
day = 24*hour
cookieSettings = defaultCookieSettings
  { cookieIsSecure = NotSecure
  , cookieXsrfSetting = Nothing
  , cookieMaxAge = Just (1*day)
  }


generateKey :: MonadIO m => m JWK
generateKey = liftIO $ Servant.generateKey


secretKey :: ByteString -> JWK
secretKey = Servant.fromSecret



protectSession :: ThrowAll api => (Session -> api) -> AuthResult Session -> api
protectSession api (Authenticated s) = api s
protectSession _ _                   = throwAll err401


protectAccount :: ThrowAll api => ((Guid Account) -> api) -> AuthResult Session -> Guid Account -> api
protectAccount api (Authenticated (Session _ _ True)) a2 = api a2
protectAccount api (Authenticated (Session _ (Just a) _)) a2
  | a == a2 = api a
  | otherwise = throwAll err401
protectAccount _ _ _ = throwAll err401




protectAdmin :: ThrowAll api => api -> AuthResult Session -> api
protectAdmin api (Authenticated Session {isAdmin}) =
  if isAdmin
    then api
    else throwAll err401
protectAdmin _ _ = throwAll err401
