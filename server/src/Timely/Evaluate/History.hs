{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Evaluate.History where


import           Data.Function                    ((&))
import qualified Data.List                        as List
import           Data.Model.Money                 (Money, fromCents, toCents)
import           Data.Text                        (Text)
import           Timely.AccountStore.Transactions (Transaction)
import qualified Timely.AccountStore.Transactions as Transaction



data Group = Group
  { name         :: Text
  , transactions :: [Transaction]
  } deriving (Eq)

instance Show Group where
  show (Group n ts) =
    "Group " ++ show n ++ " " ++ show (length ts) ++ " " ++ show (average (Group n ts))


average :: Group -> Money
average g =
  fromCents $ (toCents $ total g) `div` length (transactions g)


total :: Group -> Money
total =
  sum . map Transaction.amount . transactions



isExpense :: Transaction -> Bool
isExpense t = Transaction.amount t >= 0


isIncome :: Transaction -> Bool
isIncome t = Transaction.amount t < 0



groups :: [Transaction] -> [Group]
groups ts =
  ts & List.sortOn Transaction.name
     & List.groupBy nameEq
     & List.map toGroup
     & List.sortOn (abs.total)
     & List.reverse

  where
    nameEq :: Transaction -> Transaction -> Bool
    nameEq t1 t2 = Transaction.name t1 == Transaction.name t2


    toGroup :: [Transaction] -> Group
    toGroup (t:ts) = Group (Transaction.name t) (t:ts)
    -- this shouldn't happen
    toGroup []     = Group "" []



expenses = groups . List.filter isExpense
income   = groups . List.filter isIncome


