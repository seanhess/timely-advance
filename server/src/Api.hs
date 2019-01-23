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
import           Servant.Server.Generic (AsServerT, genericServerT)
import           Servant.Auth.Server (Auth, Cookie, CookieSettings(..), JWTSettings)

import           Auth (Phone, AuthCode)
import qualified AccountStore.Application as Application
import           AccountStore.Types (Application)
import qualified AccountStore.Account as Account
import qualified Api.Applications as Applications
import qualified Api.Sessions as Sessions
import           Api.Sessions (SetSession)
import           Api.AppM (AppM, nt, AppState(..), loadState, clientConfig, runIO)
import           Api.Types
import           Types.Guid
import           Types.Config
import           Types.Session

type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info ::      route :- Get '[JSON] Text
    , _versioned :: route :- "v1" :> ToServantApi VersionedApi
    } deriving (Generic)







data VersionedApi route = VersionedApi
    { _info     :: route :- Get '[HTML] [Link]
    , _account  :: route :- "account"      :> Auth '[Cookie] Session :> ToServantApi AccountApi
    , _app      :: route :- "application"  :> Auth '[Cookie] Session :> ToServantApi AppApi
    , _sessions :: route :- "sessions"     :> ToServantApi SessionsApi
    , _config   :: route :- "config"       :> Get '[JSON] ClientConfig
    , _config'  :: route :- "config.js"    :> Get '[JS "CONFIG"] ClientConfig
    } deriving (Generic)




-- Personal Information : Account and Application -----------------------------------------
data AccountApi route = AccountApi
    { _get   :: route :- Get '[JSON, HTML] Account
    , _banks :: route :- "bank-accounts" :> Get '[JSON, HTML] [BankAccount]
    } deriving (Generic)
    -- { _all :: route   :- Get '[JSON, HTML] [Account]
    -- , _put :: route :- Capture "id" (Id Account) :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account


data AppApi route = AppApi
    { _get    :: route :- Get '[JSON] Application
    , _result :: route :- "result" :> Get '[JSON] Result
    , _post   :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] (SetSession Application)
    } deriving (Generic)


data SessionsApi route = SessionsApi
    { _code    :: route :- ReqBody '[JSON] Phone :> Post '[JSON] NoContent
    , _auth    :: route :- Capture "phone" Phone :> ReqBody '[JSON] AuthCode :> Post '[JSON] (SetSession Session)
    , _check   :: route :- Auth '[Cookie] Session :> Get '[JSON] (Session)
    , _logout  :: route :- Delete '[JSON] (SetSession NoContent)
    } deriving Generic


-- Your own account!
accountApi :: Guid Account -> ToServant AccountApi (AsServerT AppM)
accountApi i = genericServerT AccountApi
    { _get   = run (Account.Find i) >>= notFound
    , _banks = run (Account.BankAccounts i)
    }


-- You have a validated phone number, work with that
applicationApi :: Phone -> ToServant AppApi (AsServerT AppM)
applicationApi p = genericServerT AppApi
    { _get    = run (Application.FindByPhone p) >>= notFound
    , _result = run (Application.FindResultByPhone p) >>= notFound
    , _post   = Applications.newApplication p
    }

sessionsApi :: ToServant SessionsApi (AsServerT AppM)
sessionsApi = genericServerT SessionsApi
    { _code   = Sessions.generateCode
    , _auth   = Sessions.authenticate
    , _check  = Sessions.checkSession
    , _logout = Sessions.logout
    }


baseApi :: ToServant BaseApi (AsServerT AppM)
baseApi = genericServerT BaseApi
    { _info = pure "Timely"
    , _versioned = versionedApi
    }

versionedApi :: ToServant VersionedApi (AsServerT AppM)
versionedApi = genericServerT VersionedApi
    { _account  = Sessions.protectAccount accountApi
    , _app      = Sessions.protectPhone applicationApi
    , _sessions = sessionsApi
    , _config   = clientConfig
    , _config'  = clientConfig
    , _info     = pure [Link "accounts" [], Link "applications" [], Link "config" []]
    }



apiProxy :: Proxy Api
apiProxy = Proxy


server :: AppState -> Server Api
server st = hoistServerWithContext apiProxy context (nt st) (baseApi)
  where
    context :: Proxy '[CookieSettings, JWTSettings]
    context = Proxy

-- https://haskell-servant.readthedocs.io/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html
-- https://github.com/haskell-servant/servant-auth#readme
application :: AppState -> Servant.Application
application st =
    logger $ serveWithContext apiProxy context $ server st
  where
    logger = RequestLogger.logStdout
    context = (jwtSettings st) :. (cookieSettings st) :. EmptyContext



initialize :: IO ()
initialize = do
    putStrLn "Initializing"
    state <- loadState

    runIO state $ do
      Account.initialize
      Application.initialize

    putStrLn "Done"


-- TODO use secret to generate key so sessions aren't invalidated
start :: Warp.Port -> IO ()
start port = do
    -- Load state
    state <- loadState

    putStrLn $ "Running on " ++ show port
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

