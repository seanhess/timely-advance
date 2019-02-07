{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Timely.Api.AppM
  ( AppState(..)
  , loadState
  , nt
  , AppM
  , clientConfig
  , runIO
  ) where


import           Control.Monad.Config      (MonadConfig (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Log         as Log
import           Control.Monad.Reader      (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Selda       (Selda (..))
import           Data.Pool                 (Pool)
import qualified Data.Pool                 as Pool
import           Data.String.Conversions   (cs)
import           Database.Selda            (MonadMask)
import           Database.Selda.Backend    (SeldaConnection)
import qualified Database.Selda.PostgreSQL as Selda
import qualified Network.AMQP.Worker       as Worker
import           Network.AMQP.Worker.Monad (MonadWorker (..))
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP
import           Servant                   (Handler (..), runHandler)
import           Servant.Auth.Server       (CookieSettings (..), JWTSettings)

import qualified Timely.Api.Sessions       as Sessions
import           Timely.Auth               (AuthConfig)
import qualified Timely.Auth               as Auth
import           Timely.Config             (Env (..), loadEnv)
import           Timely.Types.Config       (ClientConfig (ClientConfig), PlaidConfig (PlaidConfig))
import           Timely.Types.Secret       (Secret)
import           Timely.Types.Session      (Admin)


data AppState = AppState
    { env            :: Env
    , dbConn         :: Pool SeldaConnection
    , amqpConn       :: Worker.Connection
    , cookieSettings :: CookieSettings
    , jwtSettings    :: JWTSettings
    , manager        :: HTTP.Manager
    }




loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
    env <- loadEnv
    let sessionKey = Sessions.secretKey (sessionSecret env)
    let jwtSettings = Sessions.jwtSettings sessionKey
    let cookieSettings = Sessions.cookieSettings
    amqpConn <- Worker.connect (Worker.fromURI $ cs $ amqp env)
    dbConn <- liftIO $ Pool.createPool (createConn $ cs $ postgres env) destroyConn 1 5 3
    manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    pure AppState {..}
  where
    createConn = Selda.pgOpen' Nothing
    destroyConn = Selda.seldaClose



clientConfig :: AppM ClientConfig
clientConfig = do
    e <- asks env
    pure $ ClientConfig $ PlaidConfig (plaidPublicKey e) (plaidProducts e) (plaidEnv e)


type AppM = LogT (ReaderT AppState Handler)



instance Selda AppM where
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

instance MonadConfig (Secret Admin) AppM where
    config = asks (adminPassphrase . env)


nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT (Log.runLogT x) s


runIO :: AppState -> AppM a -> IO a
runIO s x = do
  res <- runHandler $ runReaderT (Log.runLogT x) s
  case res of
    Left err -> Prelude.error $ show err
    Right r  -> pure r
