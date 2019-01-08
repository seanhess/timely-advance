{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api.AppM
  ( AppState(..)
  , loadState
  , nt
  , AppM
  , clientConfig
  ) where


import Config (Env(..), loadEnv)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.String.Conversions (cs)
import Database.Selda (MonadMask(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import qualified Database.Selda.PostgreSQL as Selda
import qualified Network.AMQP.Worker as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid.Types as Plaid
import Servant (Handler(..), runHandler)
import Types.Config (ClientConfig(ClientConfig), PlaidConfig(PlaidConfig))


data AppState = AppState
    { env :: Env
    , dbConn :: SeldaConnection
    , amqpConn :: Worker.Connection
    -- , plaid :: Plaid.Credentials
    -- , manager :: HTTP.Manager
    }




loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
    env <- loadEnv
    amqpConn <- Worker.connect (Worker.fromURI $ cs $ amqp env)
    dbConn <- Selda.pgOpen' Nothing (cs $ postgres env)
    -- let plaid = Plaid.Credentials (plaidClientId env) (plaidClientSecret env)
    pure $ AppState {..}


clientConfig :: AppM ClientConfig
clientConfig = do
    e <- asks env
    pure $ ClientConfig $ PlaidConfig (plaidPublicKey e)


type AppM = ReaderT AppState Handler



instance MonadSelda AppM where
    seldaConnection = asks dbConn

instance MonadWorker AppM where
    amqpConnection = asks amqpConn

-- TODO upgrade to servant 0.15 and remove this
deriving instance MonadMask Handler



nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s


