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

import           Data.List                            as List (map)
import           Data.Model.Guid                      as Guid
import           Data.Model.Id                        (Token)
import           Data.Model.Money                     (Money)
import           Data.Model.Types                     (Phone, Valid)
import           Data.Number.Abs                      (Abs)
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
-- import           Servant.Server.StaticFiles           (serveDirectoryFileServer)
import           Timely.Accounts                      as Accounts
import qualified Timely.Accounts.Application          as Application
import qualified Timely.Accounts.Budgets              as Budgets
import           Timely.Accounts.Types                (Application, TransactionRow)
import qualified Timely.Accounts.Types.Subscription   as Subscription
import           Timely.Actions.AccountHealth         (AccountHealth)
import qualified Timely.Actions.AccountHealth         as AccountHealth
import           Timely.Actions.Transactions          (History)
import qualified Timely.Actions.Transactions          as Transactions
import           Timely.Advances                      (Advance)
import qualified Timely.Advances                      as Advances
import qualified Timely.Api.Admin                     as Admin
import           Timely.Api.Advances                  as Advances
import qualified Timely.Api.Applications              as Applications
import           Timely.Api.Combinators               (notFound)
import qualified Timely.Api.Health                    as ApiHealth
import           Timely.Api.Sessions                  (SetSession)
import qualified Timely.Api.Sessions                  as Sessions
import           Timely.Api.Types
import qualified Timely.Api.Webhooks                  as Webhooks
import           Timely.App                           (AppM, AppState (..), clientConfig, loadState, nt, runAppIO)
import qualified Timely.App                           as App
import           Timely.Auth                          (AuthCode)
import           Timely.Config                        (port, serveDir)
import           Timely.Evaluate.Health.Budget        (Budget, BudgetInfo)
import           Timely.Evaluate.Health.Transaction   (Expense, Income)
import qualified Timely.Transfers                     as Transfers
import           Timely.Types.Config
import           Timely.Types.Result                  (Result)
import qualified Timely.Types.Result                  as Result
import           Timely.Types.Session
import qualified Timely.Types.Update                  as Update

type Api = ToServant BaseApi AsApi


data BaseApi route = BaseApi
    { _versioned :: route :- "v1" :> ToServantApi VersionedApi
    , _health    :: route :- "health" :> Get '[PlainText] Text
    , _root      :: route :- Get '[PlainText] Text
    } deriving (Generic)






data VersionedApi route = VersionedApi
    { _info          :: route :- Get '[HTML] [Link]
    , _account       :: route :- "accounts"      :> Auth '[Cookie] Session :> Capture "id" (Guid Account) :> ToServantApi AccountApi
    , _app           :: route :- "applications"  :> Auth '[Cookie] Session :> ToServantApi AppApi
    , _sessions      :: route :- "sessions"     :> ToServantApi SessionsApi
    , _config        :: route :- "config"       :> Get '[JSON] ClientConfig
    , _config'       :: route :- "config.js"    :> Get '[JS "CONFIG"] ClientConfig
    , _hooks         :: route :- "webhooks"     :> ToServantApi WebhooksApi
    , _admin         :: route :- "admin"        :> Auth '[Cookie] Session :> ToServantApi AdminApi
    , _subscriptions :: route :- "subscriptions" :> Get '[JSON] [Subscription]
    } deriving (Generic)




-- Personal Information : Account and Application ---------------------
data AccountApi route = AccountApi
    { _get          :: route :- Get '[JSON, HTML] Account
    , _banks        :: route :- "bank-accounts" :> Get '[JSON, HTML] [BankAccount]
    , _customer     :: route :- "customer" :> Get '[JSON] Customer
    , _health       :: route :- "health"   :> Get '[JSON] (Result Update.Error AccountHealth)
    , _trans        :: route :- "transactions" :> Get '[JSON] [TransactionRow]
    , _history      :: route :- "transactions" :> "history" :> Get '[JSON] History
    , _app          :: route :- "application" :> Get '[JSON] Application
    , _advances     :: route :- "advances" :> ToServantApi AdvanceApi
    , _incomes      :: route :- "incomes"  :> ToServantApi (BudgetsApi Income)
    , _expenses     :: route :- "expenses" :> ToServantApi (BudgetsApi Expense)
    , _spending     :: route :- "spending" :> ReqBody '[JSON] (Abs Money) :> Put '[JSON] NoContent
    , _subscription :: route :- "subscription" :> ToServantApi SubscriptionApi
    } deriving (Generic)



-- expenses.... /scheduled
data BudgetsApi a route = BudgetsApi
    { _edit   :: route :- Capture "id" (Guid (Budget a)) :> ReqBody '[JSON] (BudgetInfo a) :> Put '[JSON] NoContent
    , _delete :: route :- Capture "id" (Guid (Budget a)) :> Delete '[JSON] NoContent

    , _get    :: route :- Get '[JSON] [Budget a]
    , _create :: route :- ReqBody '[JSON] (BudgetInfo a) :> Post '[JSON] (Guid (Budget a))
    } deriving (Generic)


data AdvanceApi route = AdvanceApi
    { _all    :: route :- Get '[JSON] [Advance]
    , _get    :: route :- Capture "id" (Guid Advance) :> Get '[JSON] Advance
    , _accept :: route :- Capture "id" (Guid Advance) :> "accept" :> ReqBody '[JSON] Amount :> Post '[JSON] Advance
    } deriving (Generic)


data SubscriptionApi route = SubscriptionApi
    { _get    :: route :- Get '[JSON] (Maybe Subscription)
    , _cancel :: route :- Delete '[JSON] NoContent
    , _set    :: route :- ReqBody '[JSON] Subscription.Request :> Put '[JSON] NoContent
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


data WebhooksApi route = WebhooksApi
    { _plaid :: route :- "plaid" :> ReqBody '[JSON] Webhooks.Plaid :> Post '[JSON] NoContent
    } deriving (Generic)


data AdminApi route = AdminApi
    { _test          :: route :- Get '[JSON] Text
    , _debug         :: route :- "debug" :> Get '[PlainText] Text
    , _customers     :: route :- "customers" :> Get '[JSON] [AccountCustomer]
    , _deleteAccount :: route :- "accounts" :> Capture "id" (Guid Account) :> Delete '[JSON] NoContent
    } deriving (Generic)


-- Your own account!
accountApi :: Guid Account -> ToServant AccountApi (AsServerT AppM)
accountApi i = genericServerT AccountApi
    { _get     = Accounts.find i           >>= notFound
    , _banks   = Accounts.findBanks i
    , _customer = Accounts.findCustomer i  >>= notFound
    , _app     = Application.find i        >>= notFound
    , _health  = Result.handle $ AccountHealth.analyze i
    , _trans   = Transactions.recent i
    , _history  = Transactions.history <$> Transactions.recent i
    , _advances = advanceApi i
    , _incomes = incomesApi i
    , _expenses = expensesApi i
    , _spending = \amt -> Budgets.saveSpending i amt >> pure NoContent
    , _subscription = subscriptionApi i
    }


subscriptionApi :: Guid Account -> ToServant SubscriptionApi (AsServerT AppM)
subscriptionApi i = genericServerT SubscriptionApi
    { _cancel = Accounts.subRemove i >> pure NoContent
    , _get    = Accounts.subFind i
    , _set    = \(Subscription.Request l) -> (Accounts.subSave i $ Subscription.fromLevel l) >> pure NoContent
    }
  where


incomesApi :: Guid Account -> ToServant (BudgetsApi Income) (AsServerT AppM)
incomesApi i = genericServerT BudgetsApi
   { _edit   = \bi b -> Budgets.edit i bi b    >> pure NoContent
   , _delete = \bi -> Budgets.delete i bi      >> pure NoContent
   , _create = \b -> head <$> Budgets.saveIncomes i [b]
   , _get    = Budgets.getIncomes i
   }


expensesApi :: Guid Account -> ToServant (BudgetsApi Expense) (AsServerT AppM)
expensesApi i = genericServerT BudgetsApi
   { _edit   = \bi b -> Budgets.edit i bi b     >> pure NoContent
   , _delete = \bi -> Budgets.delete i bi       >> pure NoContent
   , _create = \b -> head <$> Budgets.saveExpenses i [b]
   , _get    = Budgets.getExpenses i
   }


advanceApi :: Guid Account -> ToServant AdvanceApi (AsServerT AppM)
advanceApi i = genericServerT AdvanceApi
    { _all    =             Advances.findAll i
    , _get    = \adv     -> Advances.find i adv  >>= notFound
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


webhooksApi :: ToServant WebhooksApi (AsServerT AppM)
webhooksApi = genericServerT WebhooksApi
    { _plaid = Webhooks.plaid
    }


adminApi :: ToServant AdminApi (AsServerT AppM)
adminApi = genericServerT AdminApi
    { _test = pure "Admin!"
    , _customers = Accounts.all
    , _deleteAccount = \i -> Admin.deleteAccount i >> pure NoContent
    , _debug = App.debug
    }


baseApi :: FilePath -> ToServant BaseApi (AsServerT AppM)
baseApi _ = genericServerT BaseApi
    { _versioned = versionedApi
    , _health = ApiHealth.health
    , _root = ApiHealth.health
    }

versionedApi :: ToServant VersionedApi (AsServerT AppM)
versionedApi = genericServerT VersionedApi
    { _account  = Sessions.protectAccount accountApi
    , _app      = Sessions.protectPhone applicationApi
    , _sessions = sessionsApi
    , _config   = clientConfig
    , _config'  = clientConfig
    , _hooks    = webhooksApi
    , _info     = pure [Link "accounts" [], Link "applications" [], Link "config" []]
    , _admin    = Sessions.protectAdmin adminApi
    , _subscriptions = pure $ List.map Subscription.fromLevel $ Subscription.levels
    }



apiProxy :: Proxy Api
apiProxy = Proxy


server :: AppState -> Server Api
server st = hoistServerWithContext apiProxy context (nt st) (baseApi $ filesDir st)
  where
    context :: Proxy '[CookieSettings, JWTSettings]
    context = Proxy

    filesDir = serveDir . App.env

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

    runAppIO $ do
      Accounts.initialize
      Application.initialize
      Advances.initialize
      Transfers.initialize
      Budgets.initialize

    putStrLn "Done"


start :: IO ()
start = do
    -- Load state
    putStrLn "API"
    state <- loadState
    let p = port $ App.env state

    putStrLn $ "API | Running on " ++ show p
    Warp.run p (application state)
