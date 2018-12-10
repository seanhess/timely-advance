{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import           Prelude

import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           qualified Network.Wai.Handler.Warp as Warp

import           Servant
import           Servant.API.Generic ((:-), ToServantApi, genericApi, toServant)
import           Servant.Server.Generic (AsServerT, genericServe)

-- * Example

data AccountInfo = AccountInfo
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo


data Account = Account
    { accountId   :: Text
    , accountInfo :: AccountInfo
    } deriving (Generic, Show)

instance ToJSON Account
instance FromJSON Account


-- API specification
type BaseApi =
         Get '[JSON] Text
    :<|> "accounts" :> ToServantApi AccountsApi -- Get '[JSON] Text


data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON] [Account]
    , _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Account
    , _get :: route :- Capture "id" Text :> Get '[JSON] Account
    , _put :: route :- Capture "id" Text :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)


-- TODO make the API clickable from the browser! You can click on links in the responses
-- but it doesn't help THAT much because it can't post for you :(

accountsApi :: AccountsApi (AsServerT AppM)
accountsApi = AccountsApi
    { _all = allAccounts
    , _post = newAccount
    , _get = getAccount
    , _put = saveAccount
    }
  where
    allAccounts = return [ Account "123" fakeAccount ]
    newAccount a = return $ Account "asdf123" a
    getAccount i = return $ Account i fakeAccount
    saveAccount i a = return $ Account i a
    fakeAccount = AccountInfo "Bob" "Lewis" "bob@gmail.com"


baseApi :: ServerT BaseApi AppM
baseApi = home :<|> toServant accountsApi
  where
    home = ask


type AppCustomState = Text


type AppM = ReaderT AppCustomState Handler


nt :: AppCustomState -> AppM a -> Handler a
nt s x = runReaderT x s



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

