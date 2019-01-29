{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Timely.Advances where

import Control.Monad.Service (Service(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Selda (Selda, insert, tryCreateTable)
import           Data.Time.Clock           (UTCTime)
import qualified Data.Time.Clock as Time
import           Database.Selda            (ID, Table, SqlRow, table, Attr(..), autoPrimary, def)
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Account)
import           Timely.Types.Money        (Money)

import           Timely.Types.Guid         (Guid)


data Advance = Advance
    { advanceId :: ID Advance
    , accountId :: Guid Account
    , amount    :: Money
    , created   :: UTCTime
    , collected :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

instance SqlRow Advance





data Advances a where
    Create     :: Guid Account -> Money -> Advances Advance
    FindActive :: Guid Account -> Advances [Advance]



instance Selda m => Service m Advances where
    run (Create i amt) = create i amt
    run (FindActive i) = findActive i


advances :: Table Advance
advances =
    table "advances"
      [ #advanceId :- autoPrimary ]


create :: Selda m => Guid Account -> Money -> m Advance
create i amt = do
  time <- liftIO $ Time.getCurrentTime
  let advance = Advance { advanceId = def
                        , accountId = i
                        , amount = amt
                        , created = time
                        , collected = Nothing
                        }

  insert advances [advance]
  pure advance


findActive :: Selda m => Guid Account -> m [Advance]
findActive = undefined



initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable advances


-- created: information, but it hasn't been transfered
-- advanced: +sent money
-- collected: +received money

-- maybe the transfers should be stored separately?
-- but we can't easily tell which ones are active...
-- yeah, we can figure it out with some kind of a join
