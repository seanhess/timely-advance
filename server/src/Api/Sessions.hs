module Api.Sessions where


import           Control.Monad.Service (Service(..))
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (MonadIO)
import           Servant (NoContent(..), err401, ServantErr, Headers, Header)
import           Servant.Auth.Server (AuthResult(..), CookieSettings(..), JWTSettings, defaultJWTSettings, defaultCookieSettings, IsSecure(..), SetCookie, ThrowAll(..))
import qualified Servant.Auth.Server as Servant

import           Auth (Phone, AuthCode)
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
authenticate :: (MonadIO m, MonadError ServantErr m, Service m Account.AccountStore) => CookieSettings -> JWTSettings -> Phone -> AuthCode -> m (SetSession Session)
authenticate cke jwt p c = do
  res <- run $ Auth.CodeCheck p c
  if not res
     then throwError err401
     else session cke jwt p


session :: (MonadIO m, MonadError ServantErr m, Service m Account.AccountStore) => CookieSettings -> JWTSettings -> Phone -> m (SetSession Session)
session cke jwt p = do
    -- they've already successfully validated the code. They're in!
    ma <- run $ Account.FindByPhone p
    Auth.login cke jwt $ Session p ma


checkSession :: (MonadIO m, MonadError ServantErr m, Service m Account.AccountStore) => CookieSettings -> JWTSettings -> AuthResult Session -> m (SetSession Session)
checkSession cke jwt (Authenticated (Session p _)) = session cke jwt p
checkSession _ _ _ =
  throwError err401



logout :: Monad m => CookieSettings -> m (SetSession NoContent)
logout cke = Auth.logout cke


jwtSettings jwk = defaultJWTSettings jwk
cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing }


generateKey = Servant.generateKey



protectPhone :: ThrowAll api => (Phone -> api) -> AuthResult Session -> api
protectPhone api (Authenticated (Session p _)) = api p
protectPhone _ _ = throwAll err401


protectAccount :: ThrowAll api => ((Guid Account) -> api) -> AuthResult Session -> api
protectAccount api (Authenticated (Session _ (Just a))) = api a
protectAccount _ _ = throwAll err401

