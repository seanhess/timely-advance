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
module Timely.Accounts.Budgets where

import           Control.Effects          (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda      (Selda, deleteFrom, insert, query, tryCreateTable)
import           Data.Maybe               (listToMaybe)
import           Data.Model.Guid          (Guid)
import           Data.Model.Money         (Money)
import           Data.Number.Abs          (Abs (value), absolute)
import           Database.Selda           hiding (deleteFrom, insert, query, tryCreateTable)
import           Database.Selda.Field     (Field (..))
import           GHC.Generics             (Generic)
import           Timely.Accounts.Account  (accounts)
import           Timely.Accounts.Types    (Account)
import           Timely.Evaluate.Health   (Expense, Income)
import           Timely.Evaluate.Health.Budget (Budget(..))
import           Timely.Evaluate.Schedule (Schedule)




data BudgetType
  = Income
  | Expense
  deriving (Show, Read, Bounded, Enum, Eq, Generic)
instance SqlType BudgetType


data BudgetRow = BudgetRow
  { budgetId   :: ID BudgetRow
  , accountId  :: Guid Account
  , budgetType :: BudgetType
  , name       :: Text
  , schedule   :: Field Schedule
  , amount     :: Money
  } deriving (Show, Eq, Generic)

instance SqlRow BudgetRow






data Budgets m = BudgetsMethods
  { _setIncome   :: Guid Account -> Budget Income -> m ()
  , _setExpenses :: Guid Account -> [Budget Expense] -> m ()
  , _income      :: Guid Account -> m (Maybe (Budget Income))
  , _expenses    :: Guid Account -> m [Budget Expense]
  } deriving (Generic)

instance Effect Budgets


setIncome   :: MonadEffect Budgets m => Guid Account -> Budget Income -> m ()
income      :: MonadEffect Budgets m => Guid Account -> m (Maybe (Budget Income))
setExpenses :: MonadEffect Budgets m => Guid Account -> [Budget Expense] -> m ()
expenses    :: MonadEffect Budgets m => Guid Account -> m [Budget Expense]
BudgetsMethods setIncome setExpenses income expenses = effect




implementIO :: Selda m => RuntimeImplemented Budgets m a -> m a
implementIO = implement $
  BudgetsMethods
    (\a i -> saveBudgetsOfType Income a [i])
    (saveBudgetsOfType Expense)
    (\a -> listToMaybe <$> getBudgetsOfType Income a )
    (getBudgetsOfType Expense)








-- Selda implementation ------------------------------


budget :: Table BudgetRow
budget = table "accounts_budget"
  [ #budgetId :- autoPrimary
  , #accountId :- foreignKey accounts #accountId
  , #accountId :- index
  ]


saveBudgetsOfType :: Selda m => BudgetType -> Guid Account -> [Budget a] -> m ()
saveBudgetsOfType typ a bs = do
  deleteFrom budget
    (\b -> b ! #accountId .== literal a .&& b ! #budgetType .== literal typ)
  insert budget $ map (budgetRow a typ) bs
  pure ()


getBudgetsOfType :: Selda m => BudgetType -> Guid Account -> m [Budget a]
getBudgetsOfType typ a = do
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
