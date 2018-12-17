{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import AppM (AppM, nt, AppState(..))
import Control.Monad.Reader (asks)
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, MonadError)
import Database.Selda.PostgreSQL (pgOpen, PGConnectInfo(..))
import Database.Selda.Backend (runSeldaT)

import Types.Account
import Types.AccountInfo
import Types.Id
import qualified Endpoint.Accounts as Accounts
import GHC.Generics (Generic)

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.API.Generic ((:-), ToServantApi, ToServant, AsApi)
import Servant.Server.Generic (AsServerT, genericServerT)


type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info ::     route :- Get '[JSON] Text
    , _versioned :: route :- "v1" :> ToServantApi VersionedApi
    } deriving (Generic)


newtype VersionedApi route = VersionedApi
    { _accounts :: route :- "accounts" :> ToServantApi AccountsApi
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
    }


apiProxy :: Proxy Api
apiProxy = Proxy


application :: AppState -> Application
application st =
    serve apiProxy $ hoistServer apiProxy (nt st) baseApi


run :: Warp.Port -> IO ()
run port = do
    conn <- pgOpen $ PGConnectInfo "localhost" 5432 "postgres" Nothing (Just "postgres") Nothing
    let state = AppState "hello world"  conn

    runSeldaT Accounts.initialize conn

    putStrLn "Initialized"

    putStrLn $ "Running on " ++ show port
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

