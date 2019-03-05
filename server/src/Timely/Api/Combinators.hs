{-# LANGUAGE FlexibleContexts #-}
module Timely.Api.Combinators where

import           Control.Effects        (MonadEffect)
import           Control.Effects.Signal (throwSignal, Throw)
import           Servant                hiding (Application, Link)

notFound :: (MonadEffect (Throw ServantErr) m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing  = throwSignal err404
