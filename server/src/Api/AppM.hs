{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Api.AppM
  ( AppState(..)
  , loadState
  , nt
  , AppM
  , clientConfig
  , runIO
  ) where


import Config (Env(..), loadEnv)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Selda (Selda(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.String.Conversions (cs)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Database.Selda (MonadMask)
import Database.Selda.Backend (SeldaConnection)
import qualified Database.Selda.PostgreSQL as Selda
import qualified Network.AMQP.Worker as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import Servant (Handler(..), runHandler)
import Types.Config (ClientConfig(ClientConfig), PlaidConfig(PlaidConfig))


data AppState = AppState
    { env :: Env
    , dbConn :: Pool SeldaConnection
    , amqpConn :: Worker.Connection
    -- , plaid :: Plaid.Credentials
    -- , manager :: HTTP.Manager
    }




loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
    env <- loadEnv
    amqpConn <- Worker.connect (Worker.fromURI $ cs $ amqp env)
    dbConn <- liftIO $ Pool.createPool (createConn $ cs $ postgres env) destroyConn 1 5 3
    -- let plaid = Plaid.Credentials (plaidClientId env) (plaidClientSecret env)
    pure AppState {..}
  where
    createConn = Selda.pgOpen' Nothing
    destroyConn = Selda.seldaClose



clientConfig :: AppM ClientConfig
clientConfig = do
    e <- asks env
    pure $ ClientConfig $ PlaidConfig (plaidPublicKey e)


type AppM = ReaderT AppState Handler



instance Selda AppM where
    withConnection action = do
      pool <- asks dbConn
      Pool.withResource pool action

instance MonadWorker AppM where
    amqpConnection = asks amqpConn


nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s


runIO :: AppState -> AppM a -> IO a
runIO s x = do
  res <- runHandler $ runReaderT x s
  case res of
    Left err -> error $ show err
    Right r -> pure r
