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
  , edit
  , delete
  , saveIncomes
  , saveExpenses
  , getIncomes
  , getExpenses
  , spending
  , saveSpending
  , implementIO
  , deleteAccount
  , initialize
  ) where

import Control.Effects                    (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import Control.Monad.Selda                (Selda, deleteFrom, insert, query, tryCreateTable, update_)
import Data.List                          as List (map)
import Data.Maybe                         (listToMaybe)
import Data.Model.Guid                    as Guid (Guid, randomId)
import Data.Model.Meta                    (Meta (Meta))
import Data.Model.Money                   (Money)
import Data.Number.Abs                    (Abs (value), absolute)
import Database.Selda                     hiding (deleteFrom, insert, query, tryCreateTable, update, update_)
import Database.Selda.Field               (Field (..))
import GHC.Generics                       (Generic)
import Timely.Accounts.Account            (accounts)
import Timely.Accounts.Types              (Account)
import Timely.Evaluate.Health.Budget      (Budget(Budget), BudgetInfo(BudgetInfo), convertId, convert, convertBoth)
import Timely.Evaluate.Health.Transaction (Expense, Income)
import Timely.Evaluate.Schedule           (Schedule)




-- type BudgetWithId = Meta (Guid BudgetRow) (Budget ())


data BudgetType
  = Income
  | Expense
  deriving (Show, Read, Bounded, Enum, Eq, Generic)
instance SqlType BudgetType



data BudgetRow = BudgetRow
  { budgetId   :: Guid (Budget ())
  , accountId  :: Guid Account
  , budgetType :: BudgetType
  , name       :: Text
  , schedule   :: Field Schedule
  , amount     :: Money
  } deriving (Show, Eq, Generic)
instance SqlRow BudgetRow


data SpendingRow = SpendingRow
  { accountId :: Guid Account
  , amount    :: Money
  } deriving (Show, Eq, Generic)
instance SqlRow SpendingRow




-- these work on Budget () so I don't have to create the crazy instances
-- then the actual functions, below, work on the generic type a
data Budgets m = BudgetsMethods
  { _edit         :: Guid Account -> Guid (Budget ()) -> BudgetInfo () -> m ()
  , _delete       :: Guid Account -> Guid (Budget ()) -> m ()
  , _create       :: BudgetType -> Guid Account -> [BudgetInfo ()] -> m [Guid (Budget ())]
  , _budgets      :: BudgetType -> Guid Account -> m [Budget ()]
  , _spending     :: Guid Account -> m (Maybe (Abs Money))
  , _saveSpending :: Guid Account -> Abs Money -> m ()
  } deriving (Generic)

instance Effect Budgets


edit'    :: MonadEffect Budgets m => Guid Account -> Guid (Budget ()) -> BudgetInfo () -> m ()
delete'  :: MonadEffect Budgets m => Guid Account -> Guid (Budget ()) -> m ()
create'  :: MonadEffect Budgets m => BudgetType -> Guid Account -> [BudgetInfo ()] -> m [Guid (Budget ())]
budgets' :: MonadEffect Budgets m => BudgetType -> Guid Account -> m [Budget ()]
spending :: MonadEffect Budgets m => Guid Account -> m (Maybe (Abs Money))
saveSpending :: MonadEffect Budgets m => Guid Account -> Abs Money -> m ()
BudgetsMethods edit' delete' create' budgets' spending saveSpending = effect



edit :: MonadEffect Budgets m => Guid Account -> Guid (Budget a) -> BudgetInfo a -> m ()
edit accountId budgetId budget =
  edit' accountId (convertId budgetId) (convert budget)


delete :: MonadEffect Budgets m => Guid Account -> Guid (Budget a) -> m ()
delete accountId budgetId =
  delete' accountId (convertId budgetId)


saveIncomes :: MonadEffect Budgets m => Guid Account -> [BudgetInfo Income] -> m [Guid (Budget Income)]
saveIncomes accountId incomes =
  List.map convertId <$> create' Income accountId (List.map convert incomes)

saveExpenses :: MonadEffect Budgets m => Guid Account -> [BudgetInfo Expense] -> m [Guid (Budget Expense)]
saveExpenses accountId expenses =
  List.map convertId <$> create' Expense accountId (List.map convert expenses)


getIncomes :: MonadEffect Budgets m => Guid Account -> m [Budget Income]
getIncomes accountId =
  (List.map convertBoth) <$> budgets' Income accountId


getExpenses :: MonadEffect Budgets m => Guid Account -> m [Budget Expense]
getExpenses accountId =
  (List.map convertBoth) <$> budgets' Expense accountId


implementIO :: Selda m => RuntimeImplemented Budgets m a -> m a
implementIO = implement $
  BudgetsMethods
    updateRow
    deleteRow
    createRows
    getRows
    getSpending
    setSpending









-- Selda implementation ------------------------------


budgets :: Table BudgetRow
budgets = table "accounts_budget"
  [ #budgetId :- primary
  , #accountId :- foreignKey accounts #accountId
  , #accountId :- index
  ]


spendings :: Table SpendingRow
spendings = table "accounts_spending"
  [ #accountId :- primary
  , #accountId :- foreignKey accounts #accountId
  ]




deleteRow :: Selda m => Guid Account -> Guid (Budget a) -> m ()
deleteRow ai bi = do
  deleteFrom budgets
    (\b -> b ! #accountId .== literal ai .&& b ! #budgetId .== literal (convertId bi))
  pure ()


updateRow :: Selda m => Guid Account -> Guid (Budget ()) -> BudgetInfo () -> m ()
updateRow ai bi (BudgetInfo name schedule amount) = do
  update_ budgets
    (\b -> b ! #accountId .== literal ai .&& b ! #budgetId .== literal bi)
    (\b -> b `with` updates)
  pure ()
  where
    updates =
      [ #name     := literal name
      , #schedule := literal (Field schedule)
      , #amount   := literal (value amount)
      ]



createRows :: Selda m => BudgetType -> Guid Account -> [BudgetInfo ()] -> m [Guid (Budget ())]
createRows typ a bs = do
  rows <- mapM (budgetRow a typ) bs
  insert budgets rows
  pure $ map budgetId rows


getRows :: Selda m => BudgetType -> Guid Account -> m [Budget ()]
getRows typ a = do
  is <- query $ do
    i <- select budgets
    restrict (i ! #accountId .== literal a)
    restrict (i ! #budgetType .== literal typ)
    pure i
  pure $ map fromBudgetRow is



getSpending :: Selda m => Guid Account -> m (Maybe (Abs Money))
getSpending i = do
  ss <- query $ do
    s <- select spendings
    restrict (s ! #accountId .== literal i)
    pure (s ! #amount)
  pure $ absolute <$> listToMaybe ss

setSpending :: Selda m => Guid Account -> Abs Money -> m ()
setSpending i amount = do
  deleteFrom spendings (\a -> a ! #accountId .== literal i)
  insert spendings [SpendingRow i (value amount)]
  pure ()


-- Initialization ------------------------------------------



initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    tryCreateTable budgets
    tryCreateTable spendings


deleteAccount :: (Selda m, MonadIO m) => Guid Account -> m ()
deleteAccount i = do
  deleteFrom budgets (\b -> b ! #accountId .== literal i)
  pure ()







-- More Types ---------------------------------------------


budgetRow :: MonadIO m => Guid Account -> BudgetType -> BudgetInfo a -> m BudgetRow
budgetRow accountId budgetType (BudgetInfo name schedule amount) = do
  budgetId <- Guid.randomId
  pure $ BudgetRow
    { budgetId
    , accountId
    , budgetType
    , name
    , amount = value amount
    , schedule = Field schedule
    }


fromBudgetRow :: BudgetRow -> Budget a
fromBudgetRow BudgetRow {budgetId, name, amount, schedule} =
  Budget $ Meta (convertId budgetId) $ BudgetInfo name (field schedule) (absolute amount)


