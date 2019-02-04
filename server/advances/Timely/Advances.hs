{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Advances where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Selda       (Selda, insert, query, tryCreateTable, update_)
import           Control.Monad.Service     (Service (..))
import qualified Data.List                 as List
import           Data.Maybe                (isJust, isNothing)
import           Data.Time.Calendar        (Day)
import           Data.Time.Clock           (UTCTime)
import qualified Data.Time.Clock           as Time
import           Database.Selda            hiding (insert, query, tryCreateTable, update_)
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Account)
import           Timely.Types.Money        (Money)

import           Timely.Types.Guid         (Guid)
import qualified Timely.Types.Guid         as Guid


data Advance = Advance
    { advanceId :: Guid Advance
    , accountId :: Guid Account
    , amount    :: Money
    , due       :: Day
    , offered   :: UTCTime
    , activated :: Maybe UTCTime
    , collected :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

instance SqlRow Advance


-- CurrentOffer: The most recent offered but not accepted
-- Active: All accepted but not collected




data Advances a where
    Create        :: Guid Account -> Money -> Day -> Advances Advance
    MarkActivated :: Guid Advance -> Advances ()
    MarkCollected :: Guid Advance -> Advances ()
    FindOffer     :: Guid Account -> Advances (Maybe Advance)
    FindActive    :: Guid Account -> Advances [Advance]


instance Selda m => Service m Advances where
    run (Create i a d)    = create i a d
    run (MarkActivated i) = markActivated i
    run (MarkCollected i) = markCollected i
    run (FindOffer i)     = findOffer <$> findAdvances i
    run (FindActive i)    = findActive <$> findAdvances i


advances :: Table Advance
advances =
    table "advances"
      [ #advanceId :- primary ]


create :: Selda m => Guid Account -> Money -> Day -> m Advance
create i a d = do
    time <- liftIO $ Time.getCurrentTime
    id <- Guid.randomId
    let advance = Advance
                    { advanceId = id
                    , accountId = i
                    , amount = a
                    , due = d
                    , offered = time
                    , collected = Nothing
                    , activated = Nothing
                    }

    insert advances [advance]
    pure advance



-- | Advances in order of offer time
findAdvances :: Selda m => Guid Account -> m [Advance]
findAdvances i = query $ do
    a <- select advances
    restrict (a ! #accountId .== literal i)
    order (a ! #offered) descending
    pure a


findOffer :: [Advance] -> Maybe Advance
findOffer =
    List.find isOffer
  . List.take 1
  . List.reverse
  . List.sortOn offered


isOffer :: Advance -> Bool
isOffer a =
  isNothing (activated a)


findActive :: [Advance] -> [Advance]
findActive = List.filter isActive


isActive :: Advance -> Bool
isActive a =
  isJust (activated a) && isNothing (collected a)





markActivated :: Selda m => Guid Advance -> m ()
markActivated i = do
  time <- liftIO $ Time.getCurrentTime
  update_ advances (\a -> a ! #accountId .== literal i)
                   (\a -> a `with` [#activated := just (literal time)])


markCollected :: Selda m => Guid Advance -> m ()
markCollected i = do
  time <- liftIO $ Time.getCurrentTime
  update_ advances (\a -> a ! #accountId .== literal i)
                   (\a -> a `with` [#collected := just (literal time)])





initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable advances
