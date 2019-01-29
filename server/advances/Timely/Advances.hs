{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Advances where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Selda       (Selda, insert, query, tryCreateTable)
import           Control.Monad.Service     (Service (..))
import           Data.Time.Clock           (UTCTime)
import qualified Data.Time.Clock           as Time
import  Data.Time.Calendar (Day)
import           Database.Selda            hiding (insert, query, tryCreateTable)
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Account)
import           Timely.Types.Money        (Money)

import           Timely.Types.Guid         (Guid)


data Advance = Advance
    { advanceId :: ID Advance
    , accountId :: Guid Account
    , amount    :: Money
    , created   :: UTCTime
    , due       :: Day
    , collected :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

instance SqlRow Advance





data Advances a where
    Create     :: Guid Account -> Money -> Day -> Advances Advance
    FindActive :: Guid Account -> Advances [Advance]



instance Selda m => Service m Advances where
    run (Create i a d) = create i a d
    run (FindActive i) = findActive i


advances :: Table Advance
advances =
    table "advances"
      [ #advanceId :- autoPrimary ]


create :: Selda m => Guid Account -> Money -> Day -> m Advance
create i a d = do
    time <- liftIO $ Time.getCurrentTime
    let advance = Advance { advanceId = def
                          , accountId = i
                          , amount = a
                          , created = time
                          , due = d
                          , collected = Nothing
                          }

    insert advances [advance]
    pure advance


findActive :: Selda m => Guid Account -> m [Advance]
findActive i = query $ do
    a <- select advances
    restrict (a ! #accountId .== literal i)
    restrict (isNull $ a ! #collected )
    pure a




initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable advances


