{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
module Control.Effects.Log where

import           Control.Effects         (Effect (..), MonadEffect, RuntimeImplemented, effect, implement)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Logger    (LoggingT, MonadLogger)
import qualified Control.Monad.Logger    as Logger
import           Control.Monad.State     (MonadState, StateT)
import qualified Control.Monad.State     as State
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           GHC.Generics            (Generic)
import           Prelude                 hiding (error)


data Log m = LogMethods
    { _logInfo  :: Text -> m ()
    , _logDebug :: Text -> m ()
    , _logError :: Text -> m ()
    , _context  :: Text -> m ()
    } deriving (Generic)

instance Effect Log


info :: MonadEffect Log m => Text -> m ()
info = _logInfo effect

debug :: (MonadEffect Log m, Show a) => a -> m ()
debug a = _logDebug effect (cs $ show a)

error :: (MonadEffect Log m, Show a) => a -> m ()
error a = _logError effect (cs $ show a)

context :: MonadEffect Log m => Text -> m ()
context = _context effect


implementLogIgnore :: Applicative m => RuntimeImplemented Log m a -> m a
implementLogIgnore = implement (LogMethods ignore ignore ignore ignore)
  where
    ignore _ = pure ()


type LogT m = StateT [Text] (LoggingT m)

implementLogStdout :: MonadIO m => RuntimeImplemented Log (LogT m) a -> m a
implementLogStdout f = runLogT $ implement (LogMethods logInfo logDebug logError setContext) f
  where
    setContext :: MonadState [Text] m => Text -> m ()
    setContext t =
      State.modify $ \c -> c <> [t]

    logInfo :: (MonadState [Text] m, MonadLogger m) => Text -> m ()
    logInfo t = do
      c <- State.get
      Logger.logInfoN $ logMessage t c

    logDebug :: (MonadState [Text] m, MonadLogger m) => Text -> m ()
    logDebug t = do
      c <- State.get
      Logger.logDebugN $ logMessage t c

    logError :: (MonadState [Text] m, MonadLogger m) => Text -> m ()
    logError t = do
      c <- State.get
      Logger.logErrorN $ logMessage t c

    logMessage :: Text -> [Text] -> Text
    logMessage t ctx = Text.unwords $ ctx <> [t]

    runLogT :: MonadIO m => StateT [Text] (LoggingT m) a -> m a
    runLogT x =
      Logger.runStdoutLoggingT (State.evalStateT x [])



implementLogIO :: MonadIO m => RuntimeImplemented Log m a -> m a
implementLogIO f = implement (LogMethods logInfo logDebug logError setContext) f
  where
    setContext t = liftIO $ putStrLn $ "[Context] " <> cs t
    logInfo t = liftIO $ putStrLn $ "[Info] " <> cs t
    logDebug t = liftIO $ putStrLn $ "[Debug] " <> cs t
    logError t = liftIO $ putStrLn $ "[Error] " <> cs t
