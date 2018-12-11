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

import Data.Account
import Endpoint.Accounts
import GHC.Generics (Generic)

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.API.Generic ((:-), ToServantApi, genericApi, toServant)
import Servant.Server.Generic (AsServerT, genericServe)


-- API specification
type BaseApi =
         Get '[JSON] Text
    :<|> "accounts" :> ToServantApi AccountsApi -- Get '[JSON] Text


-- Accounts
data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON] [Account]
    , _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Account
    , _get :: route :- Capture "id" Text :> Get '[JSON] Account
    , _put :: route :- Capture "id" Text :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)


-- TODO: not servant errors
-- TODO: this approach isn't going to work, you'd need to lift all the monads when you assemble this. You would need to put it here for each function
accountsApi :: AccountsApi (AsServerT AppM)
accountsApi = AccountsApi
    { _all  = runState allAccounts
    , _post = \a -> runState $ newAccount a
    , _get  = \i -> runState (getAccount i) >>= notFound
    , _put  = \i a -> runState $ saveAccount i a
    }
  where
    -- runState :: AppM a -> Handler a
    runState :: StateT [Account] AppM a -> AppM a
    runState action = do
      var <- asks appAccounts
      as <- liftIO $ atomically $ readTVar var
      (a, as') <- runStateT action as
      liftIO $ print as'
      liftIO $ atomically $ writeTVar var as'
      return a



baseApi :: ServerT BaseApi AppM
baseApi = home :<|> accounts
  where
    home = asks appMessage
    accounts = toServant accountsApi


apiProxy :: Proxy BaseApi
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

