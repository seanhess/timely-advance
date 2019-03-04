{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module EffectsTutorial where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Effects (MonadEffect)
import Control.Effects.State (State, getState, setState, implementStateViaStateT)
import Control.Effects.List (evaluateAll, choose)
import Data.Text (Text, pack)
import Control.Effects.Reader (ReadEnv, readEnv)


addFruit :: (MonadIO m, MonadEffect (State [Text]) m) => m ()
addFruit = do
    liftIO (putStrLn "Name a type of fruit please")
    fruit <- pack <$> liftIO getLine
    knownFruits <- getState
    setState (fruit : knownFruits)


-- this doesn't work yet
main :: IO ()
main =
  evaluateAll $
    implementStateViaStateT [] $ do
      addFruit
      addFruit
      addFruit
      fruits <- getState @[Text]
      fruit <- choose fruits
      liftIO (print fruit)


getSomeInfo :: MonadEffect (ReadEnv Int) m => m Text
getSomeInfo = do
  p <- readEnv @Int
  pure $ "Port: " <> (pack $ show p)
