{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Evaluate.History where


import           Data.Aeson                         (ToJSON)
import           Data.Function                      ((&))
import qualified Data.List                          as List
import           Data.Model.Money                   (Money, fromCents, toCents)
import           Data.Number.Abs                    (Abs (value), absolute)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Timely.Evaluate.Schedule           (Schedule)
import qualified Timely.Evaluate.Schedule           as Schedule
import           Timely.Evaluate.Health.Transaction (Transaction)
import qualified Timely.Evaluate.Health.Transaction as Transaction






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
  absolute . sum . map (value . Transaction.amount)




groups :: [Transaction a] -> [Group a]
groups ts =
  ts & List.sortOn Transaction.name
     & List.groupBy nameEq
     & List.map toGroup
     & List.sortOn (value . total)
     & List.reverse

  where
    nameEq :: Transaction a -> Transaction a -> Bool
    nameEq t1 t2 = Transaction.name t1 == Transaction.name t2


    toGroup :: [Transaction a] -> Group a
    toGroup ts@(t:_) =
      Group
        (Transaction.name t)
        (transAverage ts)
        (transTotal ts)
        (Schedule.schedule $ map Transaction.date ts)
        ts
    -- this shouldn't happen
    toGroup []       = Group "" (absolute $ fromCents 0) (absolute $ fromCents 0) Nothing []



