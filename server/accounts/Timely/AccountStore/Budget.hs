{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.AccountStore.Budget where

import           Control.Effects             (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda         (Selda, deleteFrom, insert, query, tryCreateTable)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Model.Guid             (Guid)
import Data.Maybe (listToMaybe)
import           Data.Model.Money            (Money)
import           Data.Number.Abs             (Abs (value), absolute)
import           Database.Selda              hiding (deleteFrom, insert, query, tryCreateTable)
import           Database.Selda.Field        (Field (..))
import           GHC.Generics                (Generic)
import           Timely.AccountStore.Account (accounts)
import           Timely.AccountStore.Types   (Account)
import           Timely.Evaluate.Schedule    (Schedule)




data ItemType
  = Income
  | Expense
  deriving (Show, Read, Bounded, Enum, Eq, Generic)
instance SqlType ItemType


data Item (a :: ItemType) = Item
  { name     :: Text
  , amounts  :: [Abs Money]
  , schedule :: Schedule
  } deriving (Show, Eq, Generic)

instance ToJSON (Item a)
instance FromJSON (Item a)


data ItemRow = ItemRow
  { accountId :: Guid Account
  , itemType  :: ItemType
  , name      :: Text
  , schedule  :: Field Schedule
  , amounts   :: Field [Money]
  } deriving (Show, Eq, Generic)

instance SqlRow ItemRow






data Budget m = BudgetMethods
  { _setIncome   :: Guid Account -> Item 'Income -> m ()
  , _setExpenses :: Guid Account -> [Item 'Expense] -> m ()
  , _income      :: Guid Account -> m (Maybe (Item 'Income))
  , _expenses    :: Guid Account -> m [Item 'Expense]
  } deriving (Generic)

instance Effect Budget


setIncome   :: MonadEffect Budget m => Guid Account -> Item 'Income -> m ()
income      :: MonadEffect Budget m => Guid Account -> m (Maybe (Item 'Income))
setExpenses :: MonadEffect Budget m => Guid Account -> [Item 'Expense] -> m ()
expenses    :: MonadEffect Budget m => Guid Account -> m [Item 'Expense]
BudgetMethods setIncome setExpenses income expenses = effect




implementIO :: Selda m => RuntimeImplemented Budget m a -> m a
implementIO = implement $
  BudgetMethods
    (\a i -> saveItemsOfType Income a [i])
    (saveItemsOfType Expense)
    (\a -> listToMaybe <$> getItemsOfType Income a )
    (getItemsOfType Expense)








-- Selda implementation ------------------------------


budget :: Table ItemRow
budget = table "accounts_budget"
  [ #accountId :- primary
  , #accountId :- foreignKey accounts #accountId
  ]


saveItemsOfType :: Selda m => ItemType -> Guid Account -> [Item a] -> m ()
saveItemsOfType typ a items = do
  deleteFrom budget
    (\item -> item ! #accountId .== literal a .&& item ! #itemType .== literal typ)
  insert budget $ map (itemRow a typ) items
  pure ()


getItemsOfType :: Selda m => ItemType -> Guid Account -> m [Item a]
getItemsOfType typ a = do
  is <- query $ do
    i <- select budget
    restrict (i ! #accountId .== literal a)
    restrict (i ! #itemType .== literal typ)
    pure i
  pure $ map fromItemRow is





initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    tryCreateTable budget






-- More Types ---------------------------------------------


itemRow :: Guid Account -> ItemType -> Item a -> ItemRow
itemRow accountId itemType Item {name, amounts, schedule} =
  ItemRow
    { accountId
    , itemType
    , name
    , amounts = Field (map value amounts)
    , schedule = Field schedule
    }

fromItemRow :: ItemRow -> Item a
fromItemRow ItemRow {name, amounts, schedule} =
  Item
    { name
    , amounts = map absolute (field amounts)
    , schedule = field schedule
    }
