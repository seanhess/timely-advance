{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Timely.Effects.Worker where

import           Network.AMQP.Worker       (Key, Routing)
import Control.Effects (Effect(..), effect, MonadEffect)
import Control.Monad.Trans (lift)

data Publish m = PublishMethods
    { _publish :: forall a. Key Routing a -> a -> m ()
    }

publish :: MonadEffect Publish m => Key Routing a -> a -> m ()
publish = _publish effect

instance Effect Publish where
  liftThrough (PublishMethods pm) =
    PublishMethods $ \k a ->
      lift $ pm k a

  mergeContext pm =
    PublishMethods $ \k a -> do
      PublishMethods p <- pm
      p k a

