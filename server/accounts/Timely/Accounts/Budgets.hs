{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.Accounts.Budgets
  ( BudgetType(..) , Budgets(..)
  , BudgetRow
  , BudgetMeta
  , edit
  , delete
  , saveIncomes
  , saveExpenses
  , getIncomes
  , getExpenses
  , implementIO
  , deleteAccount
  , initialize
  ) where

import Control.Effects                    (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import Control.Monad.Selda                (Selda, deleteFrom, insert, query, tryCreateTable, update_)
import Data.Model.Guid                    as Guid (Guid, GuidPrefix (..), randomId)
import Data.Model.Meta                    (Meta (Meta))
import Data.Model.Money                   (Money)
import Data.Number.Abs                    (Abs (value), absolute)
import Database.Selda                     hiding (deleteFrom, insert, query, tryCreateTable, update, update_)
import Database.Selda.Field               (Field (..))
import GHC.Generics                       (Generic)
import Timely.Accounts.Account            (accounts)
import Timely.Accounts.Types              (Account)
import Timely.Evaluate.Health.Budget      (Budget (..))
import Timely.Evaluate.Health.Transaction (Expense, Income)
import Timely.Evaluate.Schedule           (Schedule)




-- type BudgetWithId = Meta (Guid BudgetRow) (Budget ())


data BudgetType
  = Income
  | Expense
  deriving (Show, Read, Bounded, Enum, Eq, Generic)
instance SqlType BudgetType



type BudgetMeta a = Meta "budgetId" (Guid BudgetRow) (Budget a)


data BudgetRow = BudgetRow
  { budgetId   :: Guid BudgetRow
  , accountId  :: Guid Account
  , budgetType :: BudgetType
  , name       :: Text
  , schedule   :: Field Schedule
  , amount     :: Money
  } deriving (Show, Eq, Generic)

instance SqlRow BudgetRow
instance GuidPrefix BudgetRow where
  guidPrefix _ = "bgt"






-- Generic Budgets API
-- this is going to be really annoying!
data Budgets m = BudgetsMethods
  { _edit    :: Guid Account -> Guid BudgetRow -> Budget () -> m ()
  , _delete  :: Guid Account -> Guid BudgetRow -> m ()
  , _create  :: BudgetType -> Guid Account -> [Budget ()] -> m [Guid BudgetRow]
  , _budgets :: BudgetType -> Guid Account -> m [BudgetMeta ()]
  } deriving (Generic)

instance Effect Budgets


edit'    :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> Budget () -> m ()
delete'  :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> m ()
create'  :: MonadEffect Budgets m => BudgetType -> Guid Account -> [Budget ()] -> m [Guid BudgetRow]
budgets' :: MonadEffect Budgets m => BudgetType -> Guid Account -> m [BudgetMeta ()]
BudgetsMethods edit' delete' create' budgets' = effect



edit :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> Budget a -> m ()
edit accountId budgetId budget =
  edit' accountId budgetId (convert budget)


delete :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> m ()
delete = delete'


saveIncomes :: MonadEffect Budgets m => Guid Account -> [Budget Income] -> m [Guid BudgetRow]
saveIncomes accountId incomes =
  create' Income accountId (map convert incomes)

saveExpenses :: MonadEffect Budgets m => Guid Account -> [Budget Expense] -> m [Guid BudgetRow]
saveExpenses accountId expenses =
  create' Expense accountId (map convert expenses)


getIncomes :: MonadEffect Budgets m => Guid Account -> m [BudgetMeta Income]
getIncomes accountId =
  map (fmap convert) <$> budgets' Income accountId


getExpenses :: MonadEffect Budgets m => Guid Account -> m [BudgetMeta Expense]
getExpenses accountId =
  map (fmap convert) <$> budgets' Expense accountId


implementIO :: Selda m => RuntimeImplemented Budgets m a -> m a
implementIO = implement $
  BudgetsMethods
    updateRow
    deleteRow
    createRows
    getRows







convert :: Budget a -> Budget b
convert Budget {name, schedule, amount} =
  Budget {name, schedule, amount}



-- Selda implementation ------------------------------


budget :: Table BudgetRow
budget = table "accounts_budget"
  [ #budgetId :- primary
  , #accountId :- foreignKey accounts #accountId
  , #accountId :- index
  ]



deleteRow :: Selda m => Guid Account -> Guid BudgetRow -> m ()
deleteRow ai bi = do
  deleteFrom budget
    (\b -> b ! #accountId .== literal ai .&& b ! #budgetId .== literal bi)
  pure ()


updateRow :: Selda m => Guid Account -> Guid BudgetRow -> Budget () -> m ()
updateRow ai bi Budget {name, schedule, amount} = do
  update_ budget
    (\b -> b ! #accountId .== literal ai .&& b ! #budgetId .== literal bi)
    (\b -> b `with` updates)
  pure ()
  where
    updates =
      [ #name     := literal name
      , #schedule := literal (Field schedule)
      , #amount   := literal (value amount)
      ]



createRows :: Selda m => BudgetType -> Guid Account -> [Budget a] -> m [Guid BudgetRow]
createRows typ a bs = do
  rows <- mapM (budgetRow a typ) bs
  insert budget rows
  pure $ map budgetId rows


getRows :: Selda m => BudgetType -> Guid Account -> m [BudgetMeta a]
getRows typ a = do
  is <- query $ do
    i <- select budget
    restrict (i ! #accountId .== literal a)
    restrict (i ! #budgetType .== literal typ)
    pure i
  pure $ map fromBudgetRow is





initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    tryCreateTable budget


deleteAccount :: (Selda m, MonadIO m) => Guid Account -> m ()
deleteAccount i = do
  deleteFrom budget (\b -> b ! #accountId .== literal i)
  pure ()





-- More Types ---------------------------------------------


budgetRow :: MonadIO m => Guid Account -> BudgetType -> Budget a -> m BudgetRow
budgetRow accountId budgetType Budget {name, amount, schedule} = do
  budgetId <- Guid.randomId
  pure $ BudgetRow
    { budgetId
    , accountId
    , budgetType
    , name
    , amount = value amount
    , schedule = Field schedule
    }

fromBudgetRow :: BudgetRow -> BudgetMeta a
fromBudgetRow BudgetRow {budgetId, name, amount, schedule} =
  Meta budgetId $
  Budget
    { name
    , amount = absolute amount
    , schedule = field schedule
    }
