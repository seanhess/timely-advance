{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import AppM (AppM, nt, AppState(..), newState)
import Control.Monad.Reader (asks)
import Control.Monad.State (StateT)
import Control.Monad.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad.Except (throwError, MonadError)
import Control.Monad.State (runStateT, get, put)

import Types.Account
import Endpoint.Accounts
import GHC.Generics (Generic)

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.API.Generic ((:-), ToServantApi, genericApi, toServant, ToServant, AsApi)
import Servant.Server.Generic (AsServerT, genericServe, AsServer, genericServerT)


type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info ::     route :- Get '[JSON] Text
    , _accounts :: route :- "accounts" :> ToServantApi AccountsApi -- Get '[JSON] Text
    } deriving (Generic)




-- Accounts --------------------------------------------------------
data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON] [Account]
    , _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Account
    , _get :: route :- Capture "id" Text :> Get '[JSON] Account
    , _put :: route :- Capture "id" Text :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)



accountsApi :: ToServant AccountsApi (AsServerT AppM)
accountsApi = genericServerT AccountsApi
    { _all  = runState allAccounts
    , _post = \a -> runState $ newAccount a
    , _get  = \i -> runState (getAccount i) >>= notFound
    , _put  = \i a -> runState $ saveAccount i a
    }
  where
    runState :: StateT [Account] AppM a -> AppM a
    runState action = do
      var <- asks appAccounts
      as <- liftIO $ atomically $ readTVar var
      (a, as') <- runStateT action as
      liftIO $ atomically $ writeTVar var as'
      return a


baseApi :: ToServant BaseApi (AsServerT AppM)
baseApi = genericServerT BaseApi
    { _info = asks appMessage
    , _accounts = accountsApi
    }


apiProxy :: Proxy Api
apiProxy = Proxy


application :: AppState -> Application
application st = do
    serve apiProxy $ hoistServer apiProxy (nt st) baseApi


run :: Warp.Port -> IO ()
run port = do
    state <- newState "a message"
    putStrLn $ "Running on " ++ (show port)
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

