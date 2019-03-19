{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
module Control.Effects.WorkerSub where

import           Control.Effects                (Effect (..), MonadEffect, RuntimeImplemented, effect, implement)
import           Control.Monad.Catch            (Exception, MonadThrow, throwM)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans            (lift)
import           Data.Aeson                     (FromJSON)
import           Data.ByteString.Lazy           (ByteString)
import           Network.AMQP.Worker            (Message, Queue)
import           Network.AMQP.Worker.Connection (Connection)
import           Network.AMQP.Worker.Message    (ConsumeResult (..), ParseError (..), Microseconds)
import qualified Network.AMQP.Worker.Message    as Message

data Subscribe m = SubscribeMethods
    { _consume :: forall a. (FromJSON a) => Queue a -> m (Maybe (Message a))
    , _consumeNext :: forall a. (FromJSON a) => Queue a -> m (Message a)
    }


consume :: (MonadEffect Subscribe m, FromJSON a) => Queue a -> m (Maybe (Message a))
consume = _consume effect

-- can I include the microseconds in the implementation? Sure
consumeNext :: (MonadEffect Subscribe m, FromJSON a) => Queue a -> m (Message a)
consumeNext = _consumeNext effect




instance Effect Subscribe where
  liftThrough methods = SubscribeMethods
    (\q -> lift $ _consume methods q)
    (\q -> lift $ _consumeNext methods q)

  mergeContext mm = SubscribeMethods
    (\q -> do
      m <- mm
      _consume m q)
    (\q -> do
      m <- mm
      _consumeNext m q)



implementAMQP :: (MonadIO m, MonadThrow m) => Connection -> Microseconds -> RuntimeImplemented Subscribe m a -> m a
implementAMQP conn interval =
  implement $ SubscribeMethods
    (consume_ conn)
    (consumeNext_ conn interval)
  where

    consumeNext_ conn interval queue = do
      res <- Message.consumeNext interval conn queue
      onResult res

    consume_ conn queue = do
      res <- Message.consume conn queue
      case res of
        Nothing -> pure Nothing
        Just a  -> Just <$> onResult a

    onResult (Parsed msg) = pure msg
    onResult (Error (ParseError e bs)) = throwM $ WorkerSubParseError e bs





data Error
  = WorkerSubParseError String ByteString
  deriving (Show)

instance Exception Error
