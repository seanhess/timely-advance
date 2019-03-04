{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Timely.Effects.Log where

import           Control.Effects     (Effect (..), MonadEffect, effect, implement, RuntimeImplemented, UniqueEffect)
import           Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.String.Conversions (cs)
import Prelude hiding (error)
import Control.Monad.State (StateT, MonadState, modify, get)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN, logDebugN, logErrorN)
import Control.Monad.IO.Class (MonadIO)


data Log m = LogMethods
    { _logInfo  :: Text -> m ()
    , _logDebug :: Text -> m ()
    , _logError :: Text -> m ()
    , _context :: Text -> m ()
    } deriving (Generic)

info :: MonadEffect Log m => Text -> m ()
info = _logInfo effect

debug :: (MonadEffect Log m, Show a) => a -> m ()
debug a = _logDebug effect (cs $ show a)

error :: (MonadEffect Log m, Show a) => a -> m ()
error a = _logError effect (cs $ show a)

context :: MonadEffect Log m => Text -> m ()
context = _context effect

ignore :: Applicative m => Text -> m ()
ignore _ = pure ()

implementLogIgnore :: Applicative m => RuntimeImplemented Log m a -> m a
implementLogIgnore = implement (LogMethods ignore ignore ignore ignore)


-- wait a minute
implementLogStdout :: (MonadState [Text] m, MonadLogger m) => RuntimeImplemented Log m a -> m a
implementLogStdout = implement (LogMethods setContext logInfo logDebug logError)
  where
    setContext :: MonadState [Text] m => Text -> m ()
    setContext t = modify $ \c -> c <> [t]

    logInfo :: (MonadState [Text] m, MonadLogger m) => Text -> m ()
    logInfo t = do
      c <- get
      logInfoN $ logMessage t c

    logDebug :: (MonadState [Text] m, MonadLogger m) => Text -> m ()
    logDebug t = do
      c <- get
      logDebugN $ logMessage t c

    logError :: (MonadState [Text] m, MonadLogger m) => Text -> m ()
    logError t = do
      c <- get
      logErrorN $ logMessage t c

    logMessage :: Text -> [Text] -> Text
    logMessage t ctx = Text.unwords $ ctx <> [t]

    -- -- they are already stateT
    -- runLogT :: MonadIO m => StateT [Text] (LoggingT m) a -> m a
    -- runLogT x = do
    --   let state = [] :: [Text]
    --   runStdoutLoggingT (evalStateT x state)
