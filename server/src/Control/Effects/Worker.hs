{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
module Control.Effects.Worker where

import           Control.Effects                (Effect (..), MonadEffect, RuntimeImplemented, effect, implement)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans            (lift)
import           Data.Aeson                     (ToJSON)
import           Network.AMQP.Worker            (Key, Routing)
import           Network.AMQP.Worker.Connection (Connection)
import qualified Network.AMQP.Worker.Message    as Message

data Publish m = PublishMethods
    { _publish :: forall a. ToJSON a => Key Routing a -> a -> m ()
    }

publish :: (MonadEffect Publish m, ToJSON a) => Key Routing a -> a -> m ()
PublishMethods publish = effect


instance Effect Publish where
  liftThrough (PublishMethods pm) =
    PublishMethods $ \k a ->
      lift $ pm k a

  mergeContext pm =
    PublishMethods $ \k a -> do
      PublishMethods p <- pm
      p k a


implementAMQP :: MonadIO m => Connection -> RuntimeImplemented Publish m a -> m a
implementAMQP conn = implement $ PublishMethods (Message.publish conn)

