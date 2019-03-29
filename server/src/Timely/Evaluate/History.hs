{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Evaluate.History where


import           Data.Aeson                        (ToJSON)
import           Data.Function                     ((&))
import qualified Data.List                         as List
import           Data.Model.Money                  (Money, fromCents, toCents)
import           Data.Number.Abs                   (Abs (value), absolute)
import           Data.Text                         (Text)
import           GHC.Generics                      (Generic)
import           Timely.Accounts.Types.Transaction (Transaction)
import qualified Timely.Accounts.Types.Transaction as Transaction




data History = History
  { income   :: [Group]
  , expenses :: [Group]
  } deriving (Show, Eq, Generic)

instance ToJSON History

data Group = Group
  { name         :: Text
  , average      :: Abs Money
  , total        :: Abs Money
  , transactions :: [Transaction]
  } deriving (Show, Eq, Generic)

instance ToJSON Group


transAverage :: [Transaction] -> Abs Money
transAverage ts =
  absolute $ fromCents $ (toCents $ value $ transTotal ts) `div` length ts


transTotal :: [Transaction] -> Abs Money
transTotal =
  absolute . sum . map (abs . Transaction.amount)



isExpense :: Transaction -> Bool
isExpense t = Transaction.amount t >= 0


isIncome :: Transaction -> Bool
isIncome t = Transaction.amount t < 0



groups :: [Transaction] -> [Group]
groups ts =
  ts & List.sortOn Transaction.name
     & List.groupBy nameEq
     & List.map toGroup
     & List.sortOn (value . total)
     & List.reverse

  where
    nameEq :: Transaction -> Transaction -> Bool
    nameEq t1 t2 = Transaction.name t1 == Transaction.name t2


    toGroup :: [Transaction] -> Group
    toGroup ts@(t:_) = Group (Transaction.name t) (transAverage ts) (transTotal ts) ts
    -- this shouldn't happen
    toGroup []       = Group "" (absolute $ fromCents 0) (absolute $ fromCents 0) []



history :: [Transaction] -> History
history ts = History
  (groups $ List.filter isIncome ts)
  (groups $ List.filter isExpense ts)
