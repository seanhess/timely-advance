{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
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


data VersionedApi route = VersionedApi
    { _info     :: route :- Get '[HTML] [Link]
    , _accounts :: route :- "accounts"     :> ToServantApi AccountsApi
    , _apps     :: route :- "applications" :> ToServantApi AppsApi
    , _sessions :: route :- "sessions"     :> ToServantApi SessionsApi
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


baseApi :: ToServant BaseApi (AsServerT AppM)
baseApi = genericServerT BaseApi
    { _info = pure "hello"
    , _versioned = versionedApi
    }

versionedApi :: ToServant VersionedApi (AsServerT AppM)
versionedApi = genericServerT VersionedApi
    { _accounts = accountsApi
    , _apps     = appsApi
    , _sessions = sessionsApi
    , _config   = clientConfig
    , _config'  = clientConfig
    , _info     = pure [Link "accounts" [], Link "applications" [], Link "config" []]
    }


apiProxy :: Proxy Api
apiProxy = Proxy


application :: AppState -> Servant.Application
application st =
    logger $ serve apiProxy $ hoistServer apiProxy (nt st) baseApi
  where
    logger = RequestLogger.logStdout


start :: Warp.Port -> IO ()
start port = do
    -- Load state
    state <- loadState

    runIO state $ do
      Account.initialize
      Application.initialize

    putStrLn $ "Running on " ++ show port
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

