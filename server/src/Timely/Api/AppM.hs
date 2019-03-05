{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Timely.Api.AppM
  ( AppState(..)
  , loadState
  , nt
  , debug
  , AppM
  , clientConfig
  , runIO
  ) where


import           Control.Effects                 (MonadEffect (..), RuntimeImplemented (..))
import           Control.Effects.Log             (Log (..), LogT, implementLogStdout)
import           Control.Effects.Signal          (Signal (..))
import           Control.Effects.Time            (Time, implementTimeIO)
import           Control.Effects.Worker          (Publish (..), implementAMQP)
import           Control.Monad.Config            (MonadConfig (..))
import           Control.Monad.Except            (MonadError (..))
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader, ReaderT, ask, asks, runReaderT)
import           Control.Monad.Selda             (Selda (..))
import           Control.Monad.Trans             (lift)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Monad.Trans.Except      (throwE)
import qualified Control.Monad.Trans.Reader      as Reader
import           Data.Function                   ((&))
import           Data.Model.Id                   (Token (..))
import           Data.Pool                       (Pool)
import qualified Data.Pool                       as Pool
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import           Database.Selda                  (MonadMask)
import           Database.Selda.Backend          (SeldaConnection)
import qualified Database.Selda.PostgreSQL       as Selda
import qualified Network.AMQP.Worker             as Worker
import           Network.AMQP.Worker.Monad       (MonadWorker (..))
import qualified Network.HTTP.Client             as HTTP
import qualified Network.HTTP.Client.TLS         as HTTP
import           Servant                         (Handler (..), ServantErr, runHandler)
import           Servant.Auth.Server             (CookieSettings (..), JWTSettings)
import qualified Text.Show.Pretty                as Pretty

-- import qualified Timely.Bank               as Bank
import           Timely.AccountStore.Application (Applications, implementApplicationsSelda)
import           Timely.AccountStore.Account (Accounts, implementAccountsSelda)
import qualified Timely.Api.Sessions             as Sessions
import           Timely.App                      (retry)
import           Timely.Auth                     (AuthConfig)
import qualified Timely.Auth                     as Auth
import           Timely.Config                   (Env (..), loadEnv, version)
import           Timely.Types.Config             (ClientConfig (ClientConfig), PlaidConfig (PlaidConfig))
import           Timely.Types.Session            (Admin)



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
    pure AppState {..}
  where
    createConn = Selda.pgOpen' Nothing
    destroyConn = Selda.seldaClose



clientConfig :: AppM ClientConfig
clientConfig = do
    e <- asks env
    pure $ ClientConfig version $ PlaidConfig (plaidPublicKey e) (plaidProducts e) (plaidEnv e)



instance (MonadBaseControl IO m, MonadMask m, MonadIO m, MonadReader AppState m) => Selda m where
    withConnection action = do
      pool <- asks dbConn
      Pool.withResource pool action

instance MonadWorker AppM where
    amqpConnection = asks amqpConn

instance MonadConfig CookieSettings AppM where
    config = asks cookieSettings

instance MonadConfig JWTSettings AppM where
    config = asks jwtSettings

instance MonadConfig AuthConfig AppM where
    config = do
      state <- ask
      let m = manager state
          u = authyBaseUrl $ env state
          k = authyApiKey $ env state
      pure $ Auth.AuthConfig m u k

instance MonadConfig (Token Admin) AppM where
    config = asks (adminPassphrase . env)

-- instance MonadConfig Bank.Config AppM where
--   config = do
--     c <- asks plaid
--     m <- asks manager
--     b <- asks (plaidBaseUrl . env)
--     pure $ Bank.Config { Bank.manager = m, Bank.baseUrl = b, Bank.credentials = c }



-- This instance is missing to get errors to work
instance MonadError ServantErr m => MonadError ServantErr (RuntimeImplemented eff m) where
  throwError e = RuntimeImplemented (lift $ throwError e)
  catchError (RuntimeImplemented m) h =
    RuntimeImplemented $ Reader.liftCatch catchError m $ (getRuntimeImplemented . h)





instance MonadEffect (Signal ServantErr b) Handler where
    effect = SignalMethods (Handler . throwE)





-- Replace AppM in run with _ to get the compiler to suggest the type for you

-- type AppM = RuntimeImplemented Time (RuntimeImplemented Publish (RuntimeImplemented Log (LogT (ReaderT AppState Handler))))

type AppT m = RuntimeImplemented Time (RuntimeImplemented Publish (RuntimeImplemented Applications (RuntimeImplemented Accounts (ReaderT AppState m))))


type AppLogT m = RuntimeImplemented Log (LogT (AppT m))


type AppM = AppLogT Handler


runAppT
  :: ( MonadIO m
     , MonadBaseControl IO m
     , MonadMask m
     )
  => AppState -> AppT m a -> m a
runAppT s x =
  let action = x
        & implementTimeIO
        & implementAMQP (amqpConn s)
        -- & implementLogStdout
        & implementApplicationsSelda
        & implementAccountsSelda
        -- & implementLogStdout
  in runReaderT action s


runAppM
  :: AppState -> AppM a -> Handler a
runAppM s x =
  runAppT s (x & implementLogStdout)



-- this is very different!
-- it can throw servant errors, rather than just arbitrary ones
-- but I'd like to unify this
runIO :: AppState -> AppM a -> IO a
runIO s x = do
  res <- runHandler $ (runAppM s x)
  case res of
    Left err -> Prelude.error $ show err
    Right r  -> pure r


nt :: AppState -> AppM a -> Handler a
nt = runAppM
