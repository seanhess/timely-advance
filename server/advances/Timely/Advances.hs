{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
module Timely.Advances
  ( Advances(..)
  , Advance(..)
  , create
  , findOffer
  , findActive
  , findAll
  , find
  , findDue
  , activate
  , markCollected
  , implementAdvancesSelda
  , Store.initialize
  ) where

import           Control.Effects          (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda      (Selda)
import           Data.Model.Guid          as Guid
import           Data.Model.Id            (Id)
import           Data.Model.Money         as Money
import           Data.Number.Abs          (Abs)
import           Data.Time.Calendar       (Day)
import           GHC.Generics             (Generic)
import           Timely.Accounts.Types    (Account)
import           Timely.Advances.Store    (Advance (..))
import qualified Timely.Advances.Store    as Store
import           Timely.Transfers.Account (TransferAccount)




-- CurrentOffer: The most recent offered but not accepted
-- Active: All accepted but not collected




data Advances m = AdvancesMethods
    { _create        :: Guid Account -> Id TransferAccount -> Abs Money -> Day -> m Advance

    , _findOffer     :: Guid Account -> m (Maybe Advance)
    , _findActive    :: Guid Account -> m [Advance]
    , _findAll       :: Guid Account -> m [Advance]
    , _find          :: Guid Account -> Guid Advance -> m (Maybe Advance)
    , _findDue       :: Day -> m [Advance]

    , _activate      :: Guid Account -> Guid Advance -> Money -> m ()
    , _markCollected :: Guid Advance -> m ()
    } deriving (Generic)

instance Effect Advances


create        :: MonadEffect Advances m => Guid Account -> Id TransferAccount -> Abs Money -> Day -> m Advance

findOffer     :: MonadEffect Advances m => Guid Account -> m (Maybe Advance)
findActive    :: MonadEffect Advances m => Guid Account -> m [Advance]
findAll       :: MonadEffect Advances m => Guid Account -> m [Advance]
find          :: MonadEffect Advances m => Guid Account -> Guid Advance -> m (Maybe Advance)
findDue       :: MonadEffect Advances m => Day -> m [Advance]

activate      :: MonadEffect Advances m => Guid Account -> Guid Advance -> Money -> m ()
markCollected :: MonadEffect Advances m => Guid Advance -> m ()
AdvancesMethods create findOffer findActive findAll find findDue activate markCollected = effect



implementAdvancesSelda :: (Selda m) => RuntimeImplemented Advances m a -> m a
implementAdvancesSelda =
  implement $
    AdvancesMethods
      Store.create
      (fmap Store.findOffer . Store.findAdvances)
      (fmap Store.findActive . Store.findAdvances)
      Store.findAdvances
      Store.findAdvance
      Store.findDue
      Store.activate
      Store.markCollected
