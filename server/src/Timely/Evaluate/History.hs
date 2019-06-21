{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Evaluate.History where


import           Data.Aeson                         (ToJSON)
import           Data.Function                      ((&))
import qualified Data.List                          as List
import           Data.Maybe                         (mapMaybe)
import           Data.Model.Money                   as Money (Money, fromCents, toCents, toFloat, fromFloat)
import           Data.Number.Abs                    (Abs (value), absolute)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Timely.Evaluate.Health             (Expense, Transaction, isBudget, BudgetInfo)
import qualified Timely.Evaluate.Health.Transaction as Trans
import           Timely.Evaluate.Schedule           (Schedule)
import qualified Timely.Evaluate.Schedule           as Schedule






data Group a = Group
  { name         :: Text
  , average      :: Abs Money
  , total        :: Abs Money
  , schedule     :: Maybe Schedule
  , transactions :: [Transaction a]
  } deriving (Show, Eq, Generic)

instance ToJSON (Group a)


transAverage :: [Transaction a] -> Abs Money
transAverage ts =
  absolute $ fromCents $ (toCents $ value $ transTotal ts) `div` length ts


transTotal :: [Transaction a] -> Abs Money
transTotal =
  absolute . sum . map (abs . Trans.amount)




groups :: [Transaction a] -> [Group a]
groups ts =
  ts & List.sortOn Trans.name
     & List.groupBy nameEq
     & List.map toGroup
     & List.sortOn (value . total)
     & List.reverse

  where
    nameEq :: Transaction a -> Transaction a -> Bool
    nameEq t1 t2 = Trans.name t1 == Trans.name t2


    toGroup :: [Transaction a] -> Group a
    toGroup ts@(t:_) =
      Group
        (Trans.name t)
        (transAverage ts)
        (transTotal ts)
        (Schedule.schedule $ map Trans.date ts)
        ts
    -- this shouldn't happen
    toGroup []       = Group "" (absolute $ fromCents 0) (absolute $ fromCents 0) Nothing []





-- | only transactions that are discretionary


data Spending


isSpending :: [BudgetInfo Expense] -> Transaction Expense -> Bool
isSpending bs t =
  not $ any (\b -> isBudget b t) bs


toSpending :: Transaction Expense -> Transaction Spending
toSpending t =
  Trans.any (Trans.name t) (Trans.amount t) (Trans.date t)



spending :: [BudgetInfo Expense] -> [Transaction Expense] -> [Transaction Spending]
spending bs ts =
  mapMaybe ifSpending ts
  where
    ifSpending :: Transaction Expense -> Maybe (Transaction Spending)
    ifSpending t =
      if isSpending bs t
        then Just (toSpending t)
        else Nothing


dailySpending :: Integer -> [Transaction Spending] -> Money
dailySpending n ts =
  let total = sum $ List.map Trans.amount ts
  in Money.fromFloat $ (Money.toFloat total) / (fromIntegral n)
