{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import AppM (AppM, nt, AppState(..))
import Events (appExchange)
import Control.Monad.Reader (asks)
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, MonadError)
import Database.Selda.PostgreSQL (pgOpen, PGConnectInfo(..))
import Database.Selda.Backend (runSeldaT)

import Types.Account
import Types.AccountInfo
import Types.Id
import Types.Config
import qualified Endpoint.Accounts as Accounts
import GHC.Generics (Generic)

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.AMQP.Worker as Worker

import Servant
import Servant.API.Generic ((:-), ToServantApi, ToServant, AsApi)
import Servant.API.ContentTypes.JS (JS)
import Servant.Server.Generic (AsServerT, genericServerT)


type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info ::      route :- Get '[JSON] Text
    , _versioned :: route :- "v1" :> ToServantApi VersionedApi
    } deriving (Generic)


data VersionedApi route = VersionedApi
    { _accounts :: route :- "accounts"  :> ToServantApi AccountsApi
    , _config   :: route :- "config"    :> Get '[JSON] ClientConfig
    , _config'  :: route :- "config.js" :> Get '[JS "CONFIG"] ClientConfig
    } deriving (Generic)



-- Accounts --------------------------------------------------------
data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON] [Account]
    , _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Account
    , _get :: route :- Capture "id" (Id Account) :> Get '[JSON] Account
    , _put :: route :- Capture "id" (Id Account) :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)


-- wait, does everything have the ability to run database code?
-- 1. I could just put the connection in the AppState, then write an adapter, like runState. I like that
-- 2. 
accountsApi :: ToServant AccountsApi (AsServerT AppM)
accountsApi = genericServerT AccountsApi
    { _all  = Accounts.allAccounts
    , _post = Accounts.newAccount
    , _get  = \i -> Accounts.findAccount i >>= notFound
    , _put  = Accounts.saveAccount
    }


baseApi :: ToServant BaseApi (AsServerT AppM)
baseApi = genericServerT BaseApi
    { _info = asks appMessage
    , _versioned = versionedApi
    }

versionedApi :: ToServant VersionedApi (AsServerT AppM)
versionedApi = genericServerT VersionedApi
    { _accounts = accountsApi
    , _config  = asks client
    , _config' = asks client
    }


apiProxy :: Proxy Api
apiProxy = Proxy


application :: AppState -> Application
application st =
    serve apiProxy $ hoistServer apiProxy (nt st) baseApi


run :: Warp.Port -> IO ()
run port = do
    amqp <- Worker.connect (Worker.fromURI "amqp://guest:guest@localhost:5672")
    db <- pgOpen $ PGConnectInfo "localhost" 5432 "postgres" Nothing (Just "postgres") Nothing

    -- TODO env config
    let config = ClientConfig (PlaidConfig "447ab26f3980c45b7202e2006dd9bf")
        state = AppState "hello world" db amqp appExchange config

    runSeldaT Accounts.initialize db

    putStrLn "Initialized"

    putStrLn $ "Running on " ++ show port
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

