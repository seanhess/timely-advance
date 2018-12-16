{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module AppM where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Database.Selda (MonadMask(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import Data.Text (Text)
import Servant (Handler)


data AppState = AppState
    { appMessage :: Text
    }



type AppM = ReaderT AppState Handler
-- type AppM = SeldaT (ReaderT AppState Handler)



-- that's not hte issue. We need to make Handler an instance of it. 
-- instance MonadMask Handler where
--     mask = liftIO . mask
--     uninterruptibleMask = liftIO . uninterruptibleMask
--     mask = liftIO . mask


nt :: SeldaConnection -> AppState -> AppM a -> Handler a
nt conn s x = undefined -- runReaderT (runSeldaT x conn) s



-- Handler = ExceptT ServantErr IO a
-- wrapped up
