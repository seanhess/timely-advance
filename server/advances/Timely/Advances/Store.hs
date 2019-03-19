{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Advances.Store where


import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Selda       (Selda, insert, query, tryCreateTable, update_)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.List                 as List
import qualified Data.Maybe                as Maybe
import           Data.Model.Guid           as Guid
import           Data.Model.Id             (Id)
import           Data.Model.Money          as Money
import           Data.Ord                  (Down (..), comparing)
import           Data.Time.Calendar        (Day)
import qualified Data.Time.Clock           as Time
import           Database.Selda            hiding (insert, query, tryCreateTable, update_)
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Account)
import           Timely.Transfers.Account  (TransferAccount)



data Advance = Advance
    { advanceId  :: Guid Advance
    , accountId  :: Guid Account
    , transferId :: Id TransferAccount
    , offer      :: Money
    , amount     :: Money
    , due        :: Day
    , offered    :: UTCTime
    , activated  :: Maybe UTCTime
    , collected  :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

instance SqlRow Advance
instance ToJSON Advance
instance FromJSON Advance
instance GuidPrefix Advance where
  guidPrefix _ = "adv"


advances :: Table Advance
advances =
    table "advances"
      [ #advanceId :- primary ]


create :: Selda m => Guid Account -> Id TransferAccount -> Money -> Day -> m Advance
create i tid a d = do
    time <- liftIO $ Time.getCurrentTime
    id <- Guid.randomId
    let advance = Advance
                    { advanceId = id
                    , accountId = i
                    , transferId = tid
                    , offer = a
                    , amount = Money.fromFloat 0
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



mostRecentAdvance :: [Advance] -> Maybe Advance
mostRecentAdvance =
    Maybe.listToMaybe
  . List.sortBy (comparing (Down . offered))


findOffer :: [Advance] -> Maybe Advance
findOffer =
    List.find isOffer
  . mostRecentAdvance


isOffer :: Advance -> Bool
isOffer a =
  Maybe.isNothing (activated a)


findActive :: [Advance] -> [Advance]
findActive = List.filter isActive


isActive :: Advance -> Bool
isActive a =
  Maybe.isJust (activated a) && Maybe.isNothing (collected a)


findAdvance :: Selda m => Guid Account -> Guid Advance -> m (Maybe Advance)
findAdvance i adv = do
    as <- query $ do
      a <- select advances
      restrict (a ! #accountId .== literal i)
      restrict (a ! #advanceId .== literal adv)
      pure a
    pure $ Maybe.listToMaybe as


findDue :: Selda m => Day -> m [Advance]
findDue d =
  query $ do
    a <- select advances
    restrict (colIsActive a .&& colIsDue a)
    pure a

  where colIsActive a =
          (isNull $ a ! #collected) .&& (not_ . isNull $ a ! #activated)

        colIsDue a =
          (a ! #due .<= literal d)



activate :: Selda m => Guid Account -> Guid Advance -> Money -> m ()
activate aid adv amt = do
  time <- liftIO $ Time.getCurrentTime
  update_ advances match (\a -> a `with` updates time)

  where match a =
          a ! #accountId .== literal aid .&& a ! #advanceId .== literal adv

        updates t =
          [ #activated := just (literal t)
          , #amount    := literal amt
          ]


markCollected :: Selda m => Guid Advance -> m ()
markCollected i = do
  time <- liftIO $ Time.getCurrentTime
  update_ advances (\a -> a ! #advanceId .== literal i)
                   (\a -> a `with` [#collected := just (literal time)])





initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable advances
