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
    { _test :: route :- Get '[JSON] Text
    } deriving (Generic)

    -- { _post :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] Account
    -- }


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


accountsApi :: AccountsApi AsServer
accountsApi = AccountsApi
    { _test = return "woot"
    }
  -- where
    -- newAccount a = return $ Account a "asdf123"


-- api :: Proxy (ToServantApi BaseApi)
-- api = genericApi (Proxy :: Proxy BaseApi)


server :: Server BaseApi
server = home :<|> test
  where
    home = return "home"

    test :: Handler Text
    test = toServant accountsApi


apiProxy :: Proxy BaseApi
apiProxy = Proxy


test :: Application
test = serve apiProxy server


run :: Warp.Port -> IO ()
run port = do
    putStrLn $ "Running on " ++ (show port)
    Warp.run port test

