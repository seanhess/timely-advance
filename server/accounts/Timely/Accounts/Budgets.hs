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
  ( BudgetType(..)
  , Budgets(..)
  , BudgetRow
  , edit
  , delete
  , saveIncomes
  , saveExpenses
  , getIncomes
  , getExpenses
  , implementIO
  , initialize
  ) where

import Control.Effects                    (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import Control.Monad.Selda                (Selda, deleteFrom, insert, query, tryCreateTable, update_)
import Data.Model.Guid                    (Guid, GuidPrefix (..))
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




data BudgetType
  = Income
  | Expense
  deriving (Show, Read, Bounded, Enum, Eq, Generic)
instance SqlType BudgetType


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
  , _create  :: BudgetType -> Guid Account -> [Budget ()] -> m ()
  , _budgets :: BudgetType -> Guid Account -> m [Budget ()]
  } deriving (Generic)

instance Effect Budgets


edit'    :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> Budget () -> m ()
delete'  :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> m ()
create'  :: MonadEffect Budgets m => BudgetType -> Guid Account -> [Budget ()] -> m ()
budgets' :: MonadEffect Budgets m => BudgetType -> Guid Account -> m [Budget ()]
BudgetsMethods edit' delete' create' budgets' = effect



edit :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> Budget a -> m ()
edit accountId budgetId budget =
  edit' accountId budgetId (convert budget)


delete :: MonadEffect Budgets m => Guid Account -> Guid BudgetRow -> m ()
delete = delete'


saveIncomes :: MonadEffect Budgets m => Guid Account -> [Budget Income] -> m ()
saveIncomes accountId incomes =
  create' Income accountId (map convert incomes)

saveExpenses :: MonadEffect Budgets m => Guid Account -> [Budget Expense] -> m ()
saveExpenses accountId expenses =
  create' Expense accountId (map convert expenses)


getIncomes :: MonadEffect Budgets m => Guid Account -> m [Budget Income]
getIncomes accountId =
  map convert <$> budgets' Income accountId


getExpenses :: MonadEffect Budgets m => Guid Account -> m [Budget Expense]
getExpenses accountId =
  map convert <$> budgets' Expense accountId


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



createRows :: Selda m => BudgetType -> Guid Account -> [Budget a] -> m ()
createRows typ a bs = do
  insert budget $ map (budgetRow a typ) bs
  pure ()


getRows :: Selda m => BudgetType -> Guid Account -> m [Budget a]
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






-- More Types ---------------------------------------------


budgetRow :: Guid Account -> BudgetType -> Budget a -> BudgetRow
budgetRow accountId budgetType Budget {name, amount, schedule} =
  BudgetRow
    { budgetId = def
    , accountId
    , budgetType
    , name
    , amount = value amount
    , schedule = Field schedule
    }

fromBudgetRow :: BudgetRow -> Budget a
fromBudgetRow BudgetRow {name, amount, schedule} =
  Budget
    { name
    , amount = absolute amount
    , schedule = field schedule
    }
