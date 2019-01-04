{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import qualified Api.Applications as Applications
import qualified Accounts.Application as App

import AppM (AppM, nt, AppState(..))
import qualified Accounts.Account as Account
import Control.Monad.Reader (asks)
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.Effect (Effect(..))
import Control.Monad.Except (throwError, MonadError)
import Database.Selda.PostgreSQL (pgOpen, PGConnectInfo(..))
import Database.Selda.Backend (runSeldaT)

-- import Types.Application as App
import Types.Account
import Types.Account.AccountInfo
import Types.Id
import Types.Config
import Types.Application
import GHC.Generics (Generic)

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.AMQP.Worker as Worker

import Servant hiding (Link, Application)
import qualified Servant
import Servant.API.Generic ((:-), ToServantApi, ToServant, AsApi)
import Servant.API.ContentTypes.JS (JS)
import Servant.API.ContentTypes.HTML (HTML, Link(..))
import Servant.Server.Generic (AsServerT, genericServerT)


type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info ::      route :- Get '[JSON] Text
    , _versioned :: route :- "v1" :> ToServantApi VersionedApi
    } deriving (Generic)


data VersionedApi route = VersionedApi
    { _info     :: route :- Get '[HTML] [Link]
    , _accounts :: route :- "accounts"     :> ToServantApi AccountsApi
    , _apps     :: route :- "applications" :> ToServantApi AppsApi
    , _config   :: route :- "config"       :> Get '[JSON] ClientConfig
    , _config'  :: route :- "config.js"    :> Get '[JS "CONFIG"] ClientConfig
    } deriving (Generic)




-- Accounts --------------------------------------------------------
data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON, HTML] [Account]
    , _get :: route :- Capture "id" (Id Account) :> Get '[JSON, HTML] (Account)
    -- , _put :: route :- Capture "id" (Id Account) :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)


data AppsApi route = AppsApi
    { _all :: route :- Get '[JSON, HTML] [Application]
    , _get :: route :- Capture "id" (Id Account) :> Get '[JSON, HTML] Application
    , _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Application
    } deriving (Generic)


accountsApi :: ToServant AccountsApi (AsServerT AppM)
accountsApi = genericServerT AccountsApi
    { _all = run (Account.All)
    , _get  = \i -> run (Account.Find i) >>= notFound
    -- , _put  = Accounts.saveAccount
    }


appsApi :: ToServant AppsApi (AsServerT AppM)
appsApi = genericServerT AppsApi
    { _all = run $ App.All
    , _get = \i -> run (App.Find i) >>= notFound
    , _post = Applications.newApplication
    }


baseApi :: ToServant BaseApi (AsServerT AppM)
baseApi = genericServerT BaseApi
    { _info = asks appMessage
    , _versioned = versionedApi
    }

versionedApi :: ToServant VersionedApi (AsServerT AppM)
versionedApi = genericServerT VersionedApi
    { _accounts = accountsApi
    , _apps     = appsApi
    , _config   = asks client
    , _config'  = asks client
    , _info     = pure $ [Link "accounts", Link "applications", Link "config"]
    }


apiProxy :: Proxy Api
apiProxy = Proxy


application :: AppState -> Servant.Application
application st =
    serve apiProxy $ hoistServer apiProxy (nt st) baseApi


start :: Warp.Port -> IO ()
start port = do
    amqp <- Worker.connect (Worker.fromURI "amqp://guest:guest@localhost:5672")
    db <- pgOpen $ PGConnectInfo "localhost" 5432 "postgres" Nothing (Just "postgres") Nothing

    -- TODO env config
    let config = ClientConfig (PlaidConfig "447ab26f3980c45b7202e2006dd9bf")
        state = AppState "hello world" db amqp config

    runSeldaT Account.initialize db

    putStrLn "Initialized"

    putStrLn $ "Running on " ++ show port
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

