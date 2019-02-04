{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Timely.Api where

import           Control.Monad.Except                 (MonadError, throwError)
import           Control.Monad.Service                (Service (..))
import           Data.Proxy                           (Proxy (..))
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Servant                              hiding (Application, Link)
import qualified Servant
import           Servant.API.ContentTypes.HTML        (HTML, Link (..))
import           Servant.API.ContentTypes.JS          (JS)
import           Servant.API.Generic                  ((:-), AsApi, ToServant,
                                                       ToServantApi)
import           Servant.Auth.Server                  (Auth, Cookie,
                                                       CookieSettings (..),
                                                       JWTSettings)
import           Servant.Server.Generic               (AsServerT,
                                                       genericServerT)

import qualified Timely.AccountStore.Account          as Account
import qualified Timely.AccountStore.Application      as Application
import           Timely.AccountStore.Types            (Application)
import           Timely.Advances                      (Advance)
import qualified Timely.Advances                      as Advances
import qualified Timely.Api.Applications              as Applications
import           Timely.Api.AppM                      (AppM, AppState (..),
                                                       clientConfig, loadState,
                                                       nt, runIO)
import           Timely.Api.Sessions                  (SetSession)
import qualified Timely.Api.Sessions                  as Sessions
import           Timely.Api.Types
import           Timely.Auth                          (AuthCode, Phone)
import           Timely.Types.Config
import           Timely.Types.Guid
import           Timely.Types.Session

type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _info      :: route :- Get '[JSON] Text
    , _versioned :: route :- "v1" :> ToServantApi VersionedApi
    } deriving (Generic)







data VersionedApi route = VersionedApi
    { _info     :: route :- Get '[HTML] [Link]
    , _account  :: route :- "accounts"      :> Auth '[Cookie] Session :> Capture "id" (Guid Account) :> ToServantApi AccountApi
    , _app      :: route :- "applications"  :> Auth '[Cookie] Session :> ToServantApi AppApi
    , _sessions :: route :- "sessions"     :> ToServantApi SessionsApi
    , _config   :: route :- "config"       :> Get '[JSON] ClientConfig
    , _config'  :: route :- "config.js"    :> Get '[JS "CONFIG"] ClientConfig
    } deriving (Generic)




-- Personal Information : Account and Application ---------------------
data AccountApi route = AccountApi
    { _get     :: route :- Get '[JSON, HTML] Account
    , _banks   :: route :- "bank-accounts" :> Get '[JSON, HTML] [BankAccount]
    , _app     :: route :- "application" :> Get '[JSON] Application
    , _result  :: route :- "application" :> "result" :> Get '[JSON] Result
    , _advance :: route :- "advances" :> ToServantApi AdvanceApi
    } deriving (Generic)


data AdvanceApi route = AdvanceApi
    { _all     :: route :- Get '[JSON] [Advance]
    , _get     :: route :- Capture "id" (Guid Advance) :> Get '[JSON] Advance
    } deriving (Generic)


data AppApi route = AppApi
    { _post   :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] (SetSession Application)
    } deriving (Generic)




data SessionsApi route = SessionsApi
    { _code   :: route :- ReqBody '[JSON] Phone :> Post '[JSON] NoContent
    , _auth   :: route :- Capture "phone" Phone :> ReqBody '[JSON] AuthCode :> Post '[JSON] (SetSession Session)
    , _check  :: route :- Auth '[Cookie] Session :> Get '[JSON] (Session)
    , _logout :: route :- Delete '[JSON] (SetSession NoContent)
    } deriving Generic


-- Your own account!
accountApi :: Guid Account -> ToServant AccountApi (AsServerT AppM)
accountApi i = genericServerT AccountApi
    { _get   = run (Account.Find i) >>= notFound
    , _banks = run (Account.BankAccounts i)
    , _app    = run (Application.Find i) >>= notFound
    , _result = run (Application.FindResult i) >>= notFound
    , _advance = advanceApi i
    }


advanceApi :: Guid Account -> ToServant AdvanceApi (AsServerT AppM)
advanceApi i = genericServerT AdvanceApi
    { _all = run (Advances.FindAll i)
    , _get = \adv -> run (Advances.Find i adv) >>= notFound
    }


-- the ONLY route that needs to authenticate via phone only is newApplication
-- EVERY other route uses an accountId, including applications
applicationApi :: Phone -> ToServant AppApi (AsServerT AppM)
applicationApi p = genericServerT AppApi
    { _post   = Applications.newApplication p
    }


sessionsApi :: ToServant SessionsApi (AsServerT AppM)
sessionsApi = genericServerT SessionsApi
    { _code   = Sessions.generateCode
    , _auth   = Sessions.authenticate
    , _check  = Sessions.checkSession
    , _logout = Sessions.logout
    }


baseApi :: ToServant BaseApi (AsServerT AppM)
baseApi = genericServerT BaseApi
    { _info = pure "Timely"
    , _versioned = versionedApi
    }

versionedApi :: ToServant VersionedApi (AsServerT AppM)
versionedApi = genericServerT VersionedApi
    { _account  = Sessions.protectAccount accountApi
    , _app      = Sessions.protectPhone applicationApi
    , _sessions = sessionsApi
    , _config   = clientConfig
    , _config'  = clientConfig
    , _info     = pure [Link "accounts" [], Link "applications" [], Link "config" []]
    }



apiProxy :: Proxy Api
apiProxy = Proxy


server :: AppState -> Server Api
server st = hoistServerWithContext apiProxy context (nt st) (baseApi)
  where
    context :: Proxy '[CookieSettings, JWTSettings]
    context = Proxy

-- https://haskell-servant.readthedocs.io/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html
-- https://github.com/haskell-servant/servant-auth#readme
application :: AppState -> Servant.Application
application st =
    logger $ serveWithContext apiProxy context $ server st
  where
    logger = RequestLogger.logStdout
    context = (jwtSettings st) :. (cookieSettings st) :. EmptyContext



initialize :: IO ()
initialize = do
    putStrLn "Initializing"
    state <- loadState

    runIO state $ do
      Account.initialize
      Application.initialize
      Advances.initialize

    putStrLn "Done"


-- TODO use secret to generate key so sessions aren't invalidated
start :: Warp.Port -> IO ()
start port = do
    -- Load state
    state <- loadState

    putStrLn $ "Running on " ++ show port
    Warp.run port (application state)


notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing  = throwError err404
