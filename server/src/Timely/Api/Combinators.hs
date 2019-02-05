{-# LANGUAGE FlexibleContexts #-}
module Timely.Api.Combinators where

import           Control.Monad.Except                 (MonadError, throwError)
import           Servant                              hiding (Application, Link)

notFound :: (MonadError ServantErr m) => Maybe a -> m a
notFound (Just a) = return a
notFound Nothing  = throwError err404
