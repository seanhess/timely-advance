{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes     #-}
module Api where

import Data.String.Conversions (cs)
import Crypto.JOSE.JWK (JWK)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Service (Service(..))
import           Control.Monad.Except (throwError, MonadError)
import           GHC.Generics (Generic)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Servant hiding (Link, Application)
import qualified Servant
import           Servant.API.Generic ((:-), ToServantApi, ToServant, AsApi)
import           Servant.API.ContentTypes.JS (JS)
import           Servant.API.ContentTypes.HTML (HTML, Link(..))
import           Servant.API.ResponseHeaders (addHeader)
import           Servant.Server.Generic (AsServerT, genericServerT)
import           Servant.Auth.Server (Auth, JWT, Cookie, FromJWT, CookieSettings(..), JWTSettings, ToJWT, generateKey, defaultJWTSettings, defaultCookieSettings, AuthResult(..), throwAll, acceptLogin, SetCookie, clearSession)

import qualified AccountStore.Application as Application
import           AccountStore.Types (Application)
import qualified AccountStore.Account as Account
import qualified Api.Applications as Applications
import           Api.AppM (AppM, nt, AppState(..), loadState, clientConfig, runIO)
import           Api.Types
import           Types.Guid
import           Types.Config
import           Types.Auth

type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info ::      route :- Get '[JSON] Text
    , _versioned :: route :- "v1" :> ToServantApi VersionedApi
    } deriving (Generic)



data Session = Session
    { accountId :: Guid Account
    } deriving (Generic, Show)

instance FromJSON Session
instance ToJSON Session
instance ToJWT UUID
instance ToJWT Session
instance FromJWT UUID
instance FromJWT Session



type Authenticated = Auth '[JWT, Cookie] Session

-- I Need to combine it with the Capture "id" (Guid Account) thing

data VersionedApi route = VersionedApi
    { _info     :: route :- Get '[HTML] [Link]
    , _accounts :: route :- "accounts"     :> ToServantApi AccountsApi
    , _apps     :: route :- "applications" :> ToServantApi AppsApi
    , _sessions :: route :- "sessions"     :> ToServantApi SessionsApi
    , _test     :: route :- "test"         :> Auth '[Cookie] Session :> Get '[JSON] Text
    , _login    :: route :- "login"        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
    , _logout   :: route :- "logout"       :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
    , _config   :: route :- "config"       :> Get '[JSON] ClientConfig
    , _config'  :: route :- "config.js"    :> Get '[JS "CONFIG"] ClientConfig
    } deriving (Generic)




-- Accounts --------------------------------------------------------
data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON, HTML] [Account]
    , _get :: route :- Capture "id" (Guid Account) :> Get '[JSON, HTML] Account
    , _banks :: route :- Capture "id" (Guid Account) :> "bank-accounts" :> Get '[JSON, HTML] [BankAccount]
    -- , _put :: route :- Capture "id" (Id Account) :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)


data AppsApi route = AppsApi
    { _all    :: route :- Get '[JSON, HTML] [Application]
    , _get    :: route :- Capture "id" (Guid Account) :> Get '[JSON, HTML] Application
    , _result :: route :- Capture "id" (Guid Account) :> "result" :> Get '[JSON] Result
    , _post   :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Application
    } deriving (Generic)


data SessionsApi route = SessionsApi
    { _code  :: route :- ReqBody '[PlainText] Phone :> Post '[PlainText] SessionId
    , _check :: route :- Capture "sessions" SessionId :> ReqBody '[PlainText] AuthCode :> Post '[PlainText] AuthToken
    } deriving Generic


accountsApi :: ToServant AccountsApi (AsServerT AppM)
accountsApi = genericServerT AccountsApi
    { _all = run Account.All
    , _get  = \i -> run (Account.Find i) >>= notFound
    , _banks = run . Account.BankAccounts
    }


appsApi :: ToServant AppsApi (AsServerT AppM)
appsApi = genericServerT AppsApi
    { _all = run Application.All
    , _get = \i -> run (Application.Find i) >>= notFound
    , _result = \i -> run (Application.FindResult i) >>= notFound
    , _post = Applications.newApplication
    }

sessionsApi :: ToServant SessionsApi (AsServerT AppM)
sessionsApi = genericServerT SessionsApi
    { _code = \_ -> pure "fake-session-id"
    , _check = \_ _ -> pure "fake-token"
    }


baseApi :: CookieSettings -> JWTSettings -> ToServant BaseApi (AsServerT AppM)
baseApi cke jwt = genericServerT BaseApi
    { _info = pure "hello"
    , _versioned = versionedApi cke jwt
    }

versionedApi :: CookieSettings -> JWTSettings -> ToServant VersionedApi (AsServerT AppM)
versionedApi cke jwt = genericServerT VersionedApi
    { _accounts = accountsApi
    , _apps     = appsApi
    , _sessions = sessionsApi
    , _test     = test
    , _login    = login cke jwt
    , _logout   = logout cke
    , _config   = clientConfig
    , _config'  = clientConfig
    , _info     = pure [Link "accounts" [], Link "applications" [], Link "config" []]
    }

test (Authenticated s) = do
  liftIO $ print s
  pure $ cs $ show s
test e = do
  liftIO $ print e
  throwAll err401


login :: CookieSettings -> JWTSettings -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie ] Text)
login cke jwt = do
  -- is this a separate key?
  let Just u = UUID.fromText "0eb9e990-a3c5-403a-a5a9-f5dd3ac2cf4b"
  mApplyCookies <- liftIO $ acceptLogin cke jwt (Session u)
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> pure $ applyCookies "Hello"

logout :: CookieSettings -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
logout cke =
  pure $ clearSession cke "ok"
-- 


apiProxy :: Proxy Api
apiProxy = Proxy


server :: CookieSettings -> JWTSettings -> AppState -> Server Api
server cke jwt st = hoistServerWithContext apiProxy context (nt st) (baseApi cke jwt)
  where
    context :: Proxy '[CookieSettings, JWTSettings]
    context = Proxy

-- https://haskell-servant.readthedocs.io/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html
application :: JWK -> AppState -> Servant.Application
application jwk st =
    logger $ serveWithContext apiProxy context $ server cke jwt st
  where
    logger = RequestLogger.logStdout
    -- context :: Context '[CookieSettings, JWTSettings]
    -- context = def :. undefined :. EmptyContext
    context = jwt :. cke :. EmptyContext
    jwt = defaultJWTSettings jwk
    cke = defaultCookieSettings { cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing }


-- hoistServerWithAuth
--   :: HasServer api '[CookieSettings, JWTSettings]
--   => Proxy api
--   -> (forall x. m x -> n x)
--   -> ServerT api m
--   -> ServerT api n
-- hoistServerWithAuth api =
--   hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings])


start :: Warp.Port -> IO ()
start port = do
    -- Load state
    state <- loadState

    runIO state $ do
      Account.initialize
      Application.initialize

    putStrLn $ "Running on " ++ show port
    key <- generateKey
    Warp.run port (application key state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

