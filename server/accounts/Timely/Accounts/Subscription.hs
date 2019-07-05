{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.Accounts.Subscription where


import Control.Monad.Selda                (Selda, deleteFrom, insert, query, tryCreateTable)
import Data.Maybe                         (listToMaybe)
import Data.Model.Guid                    (Guid)
import Data.Model.Money                   (Money)
import Data.Time.Clock                    as Time (getCurrentTime)
import Database.Selda                     hiding (deleteFrom, insert, limit, query, tryCreateTable)
import Timely.Accounts.Account            (accounts)
import Timely.Accounts.Types              (Account, Subscription (..))
import Timely.Accounts.Types.Subscription as Subscription (Level)







-- Selda implementation ------------------------------

data SubscriptionRow = SubscriptionRow
  { accountId :: Guid Account
  , level     :: Subscription.Level
  , limit     :: Money
  , cost      :: Money
  , created   :: UTCTime
  } deriving (Show, Eq, Generic)

instance SqlRow SubscriptionRow



subscriptions :: Table SubscriptionRow
subscriptions = table "accounts_subscriptions"
  [ #accountId :- primary
  , #accountId :- foreignKey accounts #accountId
  ]



save :: Selda m => Guid Account -> Subscription -> m ()
save i s = do
    now <- liftIO $ Time.getCurrentTime
    remove i
    insert subscriptions [toRow now i s]
    pure ()


remove :: Selda m => Guid Account -> m ()
remove i = do
    deleteFrom subscriptions (\x -> x ! #accountId .== literal i)
    pure ()


find :: Selda m => Guid Account -> m (Maybe Subscription)
find i = do
  ss <- query $ do
    s <- select subscriptions
    restrict (s ! #accountId .== literal i)
    pure s
  pure $ fromRow <$> listToMaybe ss



initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    tryCreateTable subscriptions




toRow :: UTCTime -> Guid Account -> Subscription -> SubscriptionRow
toRow created accountId Subscription {cost, limit, level} =
  SubscriptionRow { accountId, cost, limit, created, level }


fromRow :: SubscriptionRow -> Subscription
fromRow SubscriptionRow { cost, limit, level } =
  Subscription { cost, limit, level }












