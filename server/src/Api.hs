{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import AppM (AppM, nt)
import Control.Monad.Reader (ask)
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
    runState action = do
      (a, _) <- runStateT action []
      return a



baseApi :: ServerT BaseApi AppM
baseApi = home :<|> accounts
  where
    home = ask
    accounts = toServant accountsApi


apiProxy :: Proxy BaseApi
apiProxy = Proxy


test :: Application
test = do
    let cfg = "config"
    serve apiProxy $ hoistServer apiProxy (nt cfg) baseApi


run :: Warp.Port -> IO ()
run port = do
    putStrLn $ "Running on " ++ (show port)
    Warp.run port test


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing = throwError err404

