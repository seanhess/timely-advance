{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Timely.App.AppM
  ( AppState(..)
  , loadState
  , nt
  , debug
  , AppM, AppT
  , clientConfig
  , runApp
  , runAppIO
  ) where


import           Control.Effects              (MonadEffect (..), RuntimeImplemented (..))
import           Control.Effects.Async        (Async)
import qualified Control.Effects.Async        as Async
import           Control.Effects.Log          (Log (..), LogT)
import qualified Control.Effects.Log          as Log
import           Control.Effects.Signal       (Signal (..))
import           Control.Effects.Time         (Time)
import qualified Control.Effects.Time         as Time
import           Control.Effects.Worker       (Publish (..))
import qualified Control.Effects.Worker       as Worker
import           Control.Monad.Config         (MonadConfig (..))
import           Control.Monad.Except         (MonadError (..))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, asks, runReaderT)
import           Control.Monad.Selda          (Selda (..))
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Except   (throwE)
import qualified Control.Monad.Trans.Reader   as Reader
import           Data.Function                ((&))
import           Data.Model.Id                (Token (..))
import           Data.Pool                    (Pool)
import qualified Data.Pool                    as Pool
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Database.Selda               (MonadMask)
import           Database.Selda.Backend       (SeldaConnection)
import qualified Database.Selda.PostgreSQL    as Selda
import qualified Network.AMQP.Worker          as Worker
import qualified Network.Dwolla               as Dwolla
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Client.TLS      as HTTP
import qualified Network.Plaid                as Plaid
import           Servant                      (Handler (..), ServantErr, runHandler)
import           Servant.Auth.Server          (CookieSettings (..), JWTSettings)
import qualified Text.Show.Pretty             as Pretty
import           Timely.Accounts              (Accounts)
import qualified Timely.Accounts              as Accounts
import           Timely.Accounts.Application  (Applications)
import qualified Timely.Accounts.Application  as Applications
import           Timely.Accounts.Budget       (Budget)
import qualified Timely.Accounts.Budget       as Budget
import           Timely.Advances              (Advances)
import qualified Timely.Advances              as Advances
import qualified Timely.Api.Sessions          as Sessions
import           Timely.App.Retry             (retry)
import           Timely.Auth                  (AuthConfig)
import           Timely.Auth                  (Auth)
import qualified Timely.Auth                  as Auth
import           Timely.Bank                  (Banks)
import qualified Timely.Bank                  as Bank
import           Timely.Config                (Env (..), loadEnv, version)
import           Timely.Notify                (Notify)
import qualified Timely.Notify                as Notify
import           Timely.Transfers             (Transfers)
import qualified Timely.Transfers             as Transfers
import           Timely.Types.Config          (ClientConfig (ClientConfig), PlaidConfig (PlaidConfig))
import           Timely.Types.Session         (Admin)
import           Timely.Underwriting          (Underwriting)
import qualified Timely.Underwriting          as Underwriting



data AppState = AppState
    { env            :: Env
    , dbConn         :: Pool SeldaConnection
    , amqpConn       :: Worker.Connection
    , cookieSettings :: CookieSettings
    , jwtSettings    :: JWTSettings
    , manager        :: HTTP.Manager
    }




debug :: AppM Text
debug = do
  e <- asks env
  pure $ cs $ Pretty.dumpStr e



loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
    env <- loadEnv
    let sessionKey = Sessions.secretKey (sessionSecret env)
    let jwtSettings = Sessions.jwtSettings sessionKey
    let cookieSettings = Sessions.cookieSettings
    amqpConn <- retry $ Worker.connect (Worker.fromURI $ cs $ amqp env)
    dbConn <- liftIO $ Pool.createPool (createConn $ cs $ postgres env) destroyConn 1 5 3
    manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    pure AppState {dbConn, amqpConn, manager, env, cookieSettings, jwtSettings}
  where
    createConn = Selda.pgOpen' Nothing
    destroyConn = Selda.seldaClose



clientConfig :: AppM ClientConfig
clientConfig = do
    e <- asks env
    pure $ ClientConfig version $ PlaidConfig (plaidPublicKey e) (plaidProducts e) (plaidEnv e) (plaidWebhook e)



instance (MonadBaseControl IO m, MonadMask m, MonadIO m, MonadReader AppState m) => Selda m where
    withConnection action = do
      pool <- asks dbConn
      Pool.withResource pool action

instance Monad m => MonadConfig CookieSettings (AppT m) where
    config = asks cookieSettings

instance Monad m => MonadConfig JWTSettings (AppT m) where
    config = asks jwtSettings

instance (Monad m, MonadReader AppState m) => MonadConfig AuthConfig m where
    config = do
      state <- ask
      let m = manager state
          u = authyBaseUrl $ env state
          k = authyApiKey $ env state
      pure $ Auth.AuthConfig m u k

instance Monad m => MonadConfig (Token Admin) (AppT m) where
    config = asks (adminPassphrase . env)

instance (Monad m, MonadReader AppState m) => MonadConfig Bank.Config m where
  config = do
    m <- asks manager
    c <- asks (plaidClientId . env)
    s <- asks (plaidClientSecret . env)
    b <- asks (plaidBaseUrl . env)
    pure $ Bank.Config { Bank.manager = m, Bank.baseUrl = b, Bank.credentials = Plaid.Credentials c s }

instance (Monad m, MonadReader AppState m) => MonadConfig Dwolla.Credentials m where
  config = do
    e <- asks env
    pure $ Dwolla.Credentials (dwollaClientId e) (dwollaSecret e)

instance (Monad m, MonadReader AppState m) => MonadConfig Transfers.Config m where
  config = do
    dwolla <- config
    mgr <- asks manager
    base <- asks (dwollaBaseUrl . env)
    auth <- asks (dwollaAuthBaseUrl . env)
    src <- asks (dwollaFundingSource . env)
    pure $ Transfers.Config src (Dwolla.Config mgr base auth dwolla)

instance (Monad m, MonadReader AppState m) => MonadConfig Notify.Config m where
  config = do
    e <- asks env
    pure $ Notify.Config (twilioFromPhone e) (twilioAccountId e) (twilioAuthToken e) (appEndpoint e)


-- This instance is missing to get errors to work
instance MonadError ServantErr m => MonadError ServantErr (RuntimeImplemented eff m) where
  throwError e = RuntimeImplemented (lift $ throwError e)
  catchError (RuntimeImplemented m) h =
    RuntimeImplemented $ Reader.liftCatch catchError m $ (getRuntimeImplemented . h)


instance MonadEffect (Signal ServantErr b) Handler where
    effect = SignalMethods (Handler . throwE)




type AppT m = RuntimeImplemented Log (LogT (RuntimeImplemented Time (RuntimeImplemented Publish (RuntimeImplemented Applications (RuntimeImplemented Accounts (RuntimeImplemented Banks (RuntimeImplemented Advances (RuntimeImplemented Auth (RuntimeImplemented Transfers (RuntimeImplemented Notify (RuntimeImplemented Underwriting (RuntimeImplemented Async (RuntimeImplemented Budget (ReaderT AppState m))))))))))))))

type AppM = AppT Handler


runApp
  :: ( MonadIO m
     , MonadBaseControl IO m
     , MonadMask m
     )
  => AppState -> AppT m a -> m a
runApp s x =
  let action = x
        & Log.implementStdout
        & Time.implementIO
        & Worker.implementAMQP (amqpConn s)
        & Applications.implementIO
        & Accounts.implementIO
        & Bank.implementBankIO
        & Advances.implementAdvancesSelda
        & Auth.implementIO
        & Transfers.implementIO
        & Notify.implementIO
        & Underwriting.implementMock
        & Async.implementIO
        & Budget.implementIO
  in runReaderT action s



runAppIO :: AppM a -> IO a
runAppIO x = do
  s <- loadState
  res <- runHandler $ runApp s x
  case res of
    Left err -> Prelude.error $ show err
    Right r  -> pure r


nt :: AppState -> AppM a -> Handler a
nt s x = runApp s x
