{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppM where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Database.Selda (MonadMask(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import Data.Text (Text)
import Servant (Handler(..), runHandler)


data AppState = AppState
    { appMessage :: Text
    , connection :: SeldaConnection
    }



type AppM = ReaderT AppState Handler



instance MonadSelda AppM where
    seldaConnection = asks connection
    -- wrapTransaction = error "TODO: upgrade servant to get this for free"

deriving instance MonadMask Handler
-- instance MonadMask Handler where



nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s


