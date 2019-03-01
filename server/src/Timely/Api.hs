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

import           Control.Monad.Service                (Service (..))
import           Data.Model.Guid                      as Guid
import           Data.Model.Id                        (Token)
import           Data.Model.Types                     (Phone, Valid)
import           Data.Proxy                           (Proxy (..))
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Servant                              hiding (Application, Link)
import qualified Servant
import           Servant.API.ContentTypes.HTML        (HTML, Link (..))
import           Servant.API.ContentTypes.JS          (JS)
import           Servant.API.Generic                  ((:-), AsApi, ToServant, ToServantApi)
import           Servant.Auth.Server                  (Auth, Cookie, CookieSettings (..), JWTSettings)
import           Servant.Server.Generic               (AsServerT, genericServerT)
import           Servant.Server.StaticFiles           (serveDirectoryFileServer)
import qualified Timely.AccountStore.Account          as Account
import qualified Timely.AccountStore.Application      as Application
import           Timely.AccountStore.Types            (AppResult, Application)
import           Timely.Advances                      (Advance)
import qualified Timely.Advances                      as Advances
import           Timely.Api.Advances                  as Advances
import qualified Timely.Api.Health as Health
import qualified Timely.Api.Applications              as Applications
import           Timely.Api.AppM                      (AppM, AppState (..), clientConfig, loadState, nt, runIO)
import qualified Timely.Api.AppM                      as AppM
import           Timely.Api.Combinators               (notFound)
import           Timely.Api.Sessions                  (SetSession)
import qualified Timely.Api.Sessions                  as Sessions
import           Timely.Api.Types
import           Timely.Auth                          (AuthCode)
import           Timely.Config                        (port, serveDir)
import qualified Timely.Transfers                     as Transfers
import           Timely.Types.Config
import           Timely.Types.Session

type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _versioned :: route :- "v1" :> ToServantApi VersionedApi
    , _health    :: route :- "health" :> Get '[PlainText] Text
    , _debug     :: route :- "debug" :> Get '[PlainText] Text
    , _files     :: route :- Raw
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
    { _get      :: route :- Get '[JSON, HTML] Account
    , _banks    :: route :- "bank-accounts" :> Get '[JSON, HTML] [BankAccount]
    , _app      :: route :- "application" :> Get '[JSON] Application
    , _result   :: route :- "application" :> "result" :> Get '[JSON] AppResult
    , _advances :: route :- "advances" :> ToServantApi AdvanceApi
    } deriving (Generic)


data AdvanceApi route = AdvanceApi
    { _all    :: route :- Get '[JSON] [Advance]
    , _get    :: route :- Capture "id" (Guid Advance) :> Get '[JSON] Advance
    , _accept :: route :- Capture "id" (Guid Advance) :> "accept" :> ReqBody '[JSON] Amount :> Post '[JSON] Advance
    } deriving (Generic)


data AppApi route = AppApi
    { _post   :: route :- ReqBody '[JSON] AccountInfo :> Post '[JSON] (SetSession Application)
    } deriving (Generic)




data SessionsApi route = SessionsApi
    -- admin must be before auth
    { _admin  :: route :- "admin" :> ReqBody '[JSON] (Token Admin) :> Post '[JSON] (SetSession Session)
    , _code   :: route :- ReqBody '[JSON] (Valid Phone) :> Post '[JSON] NoContent
    , _auth   :: route :- Capture "phone" (Valid Phone) :> ReqBody '[JSON] AuthCode :> Post '[JSON] (SetSession Session)
    , _check  :: route :- Auth '[Cookie] Session :> Get '[JSON] (Session)
    , _logout :: route :- Delete '[JSON] (SetSession NoContent)
    } deriving Generic


-- Your own account!
accountApi :: Guid Account -> ToServant AccountApi (AsServerT AppM)
accountApi i = genericServerT AccountApi
    { _get     = run (Account.Find i)           >>= notFound
    , _banks   = run (Account.BankAccounts i)
    , _app     = run (Application.Find i)       >>= notFound
    , _result  = run (Application.FindResult i) >>= notFound
    , _advances = advanceApi i
    }


advanceApi :: Guid Account -> ToServant AdvanceApi (AsServerT AppM)
advanceApi i = genericServerT AdvanceApi
    { _all    =             run (Advances.FindAll i)
    , _get    = \adv     -> run (Advances.Find i adv)                  >>= notFound
    , _accept = acceptAdvance i
    }


-- the ONLY route that needs to authenticate via phone only is newApplication
-- EVERY other route uses an accountId, including applications
applicationApi :: Valid Phone -> ToServant AppApi (AsServerT AppM)
applicationApi p = genericServerT AppApi
    { _post   = Applications.newApplication p
    }


sessionsApi :: ToServant SessionsApi (AsServerT AppM)
sessionsApi = genericServerT SessionsApi
    { _code   = Sessions.generateCode
    , _auth   = Sessions.authenticate
    , _check  = Sessions.checkSession
    , _logout = Sessions.logout
    , _admin  = Sessions.authAdmin
    }


baseApi :: FilePath -> ToServant BaseApi (AsServerT AppM)
baseApi p = genericServerT BaseApi
    { _versioned = versionedApi
    , _files = serveDirectoryFileServer p
    , _debug = AppM.debug
    , _health = Health.health
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
server st = hoistServerWithContext apiProxy context (nt st) (baseApi $ filesDir st)
  where
    context :: Proxy '[CookieSettings, JWTSettings]
    context = Proxy

    filesDir = serveDir . AppM.env

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
      Transfers.initialize

    putStrLn "Done"


-- TODO use secret to generate key so sessions aren't invalidated
start :: IO ()
start = do
    -- Load state
    putStrLn "API"
    state <- loadState
    let p = port $ AppM.env state

    putStrLn $ "API | Running on " ++ show p
    Warp.run p (application state)
