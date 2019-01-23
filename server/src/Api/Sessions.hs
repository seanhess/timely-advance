module Api.Sessions where


import           Crypto.JOSE.JWK (JWK)
import           Control.Monad.Config
import           Control.Monad.Service (Service(..))
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Servant (NoContent(..), err401, ServantErr, Headers, Header)
import           Servant.Auth.Server (AuthResult(..), CookieSettings(..), JWTSettings, defaultJWTSettings, defaultCookieSettings, IsSecure(..), SetCookie, ThrowAll(..))
import qualified Servant.Auth.Server as Servant

import           Auth (Phone, AuthCode, AuthConfig)
import qualified Auth
import qualified AccountStore.Account as Account
import           AccountStore.Types (Account)
import           Types.Session (Session(..))
import           Types.Guid


type SetSession a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a



generateCode :: Service m Auth.AuthService => Phone -> m NoContent
generateCode p = do
  run $ Auth.CodeGenerate p
  pure NoContent


-- TODO check to see if there's an account and set the account id
authenticate
  :: ( MonadIO m
     , MonadError ServantErr m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     , MonadConfig AuthConfig m
     , Service m Account.AccountStore
     ) => Phone -> AuthCode -> m (SetSession Session)
authenticate p c = do
  res <- run $ Auth.CodeCheck p c
  if not res
     then throwError err401
     else session p


session
  :: ( MonadIO m
     , MonadError ServantErr m
     , Service m Account.AccountStore
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Phone -> m (SetSession Session)
session p = do
    -- they've already successfully validated the code. They're in!
    ma <- run $ Account.FindByPhone p
    let s = Session p ma
    setSession s s


setSession
  :: ( MonadIO m
     , MonadError ServantErr m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Session -> value -> m (SetSession value)
setSession s value = do
    cke <- config
    jwt <- config
    Auth.login cke jwt s value



checkSession
  :: ( MonadIO m
     , MonadError ServantErr m
     , Service m Account.AccountStore
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => AuthResult Session -> m (Session)
checkSession (Authenticated s) = pure s
checkSession _ =
  throwError err401



logout :: (Monad m, MonadConfig CookieSettings m) => m (SetSession NoContent)
logout = do
  cke <- config
  Auth.logout cke


jwtSettings jwk = defaultJWTSettings jwk
cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing }


generateKey :: MonadIO m => m JWK
generateKey = liftIO $ Servant.generateKey



protectPhone :: ThrowAll api => (Phone -> api) -> AuthResult Session -> api
protectPhone api (Authenticated (Session p _)) = api p
protectPhone _ _ = throwAll err401


protectAccount :: ThrowAll api => ((Guid Account) -> api) -> AuthResult Session -> Guid Account -> api
protectAccount api (Authenticated (Session _ (Just a))) a2
  | a == a2 = api a
  | otherwise = throwAll err401
protectAccount _ _ _ = throwAll err401

