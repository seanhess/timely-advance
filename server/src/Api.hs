{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import           Prelude

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           qualified Network.Wai.Handler.Warp as Warp

import           Servant
import           Servant.API.Generic ((:-), ToServantApi, genericApi, toServant)
import           Servant.Server.Generic (AsServer, genericServe)

-- * Example

data AccountInfo = AccountInfo
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo


-- the official account might have all kinds of metadata
-- multiple rows, etc
-- what other info will it have?
data Account = Account
    { accountInfo :: AccountInfo
    , accountId   :: Text
    } deriving (Generic, Show)

instance ToJSON Account
instance FromJSON Account


-- API specification
type BaseApi =
         Get '[JSON] Text
    :<|> "accounts" :> ToServantApi AccountsApi -- Get '[JSON] Text
    -- , _accounts :: route :- "accounts" :> ToServantApi AccountsApi
    -- , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool


data AccountsApi route = AccountsApi
    { _all :: route :- Get '[JSON] [Account]
    , _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Account
    , _get :: route :- Capture "id" Text :> Get '[JSON] Account
    , _put :: route :- Capture "id" Text :> ReqBody '[JSON] AccountInfo :> Put '[JSON] Account
    } deriving (Generic)


       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       -- "accounts" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
       -- DELETE /greet/:greetid
  -- :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

-- baseApi :: BaseApi AsServer
-- baseApi = BaseApi
--     { _test = return "Hello"
--     , _accounts = accountsApi
--     }

-- TODO make the API clickable from the browser! You can click on links in the responses

accountsApi :: AccountsApi AsServer
accountsApi = AccountsApi
    { _all = allAccounts
    , _post = newAccount
    , _get = getAccount
    , _put = saveAccount
    }
  where
    allAccounts = return [ Account fakeAccount "1234"]
    newAccount a = return $ Account a "asdf123"
    getAccount i = return $ Account fakeAccount i
    saveAccount i a = return $ Account a i
    fakeAccount = AccountInfo "Bob" "Lewis" "bob@gmail.com"


-- api :: Proxy (ToServantApi BaseApi)
-- api = genericApi (Proxy :: Proxy BaseApi)


server :: Server BaseApi
server = home :<|> toServant accountsApi
  where
    home = return "home"


apiProxy :: Proxy BaseApi
apiProxy = Proxy


test :: Application
test = serve apiProxy server


run :: Warp.Port -> IO ()
run port = do
    putStrLn $ "Running on " ++ (show port)
    Warp.run port test

