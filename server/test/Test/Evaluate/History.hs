{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.History where

import qualified Data.List                          as List
import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (absolute)
import           Test.Evaluate.History.Transactions (allTransactions, transaction, expenses, incomes)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Evaluate.Health.Transaction (Transaction (..))
import qualified Timely.Evaluate.Health.Transaction as Trans
import qualified Timely.Evaluate.History            as History

tests :: Tests ()
tests = do
  group "separate" $ do
    test "should be either expenses or income" $ do
      length expenses + length incomes @?= length allTransactions

    test "should find some expenses" $ do
      length expenses > 0 @? "0"

    test "should find some income" $ do
      length incomes > 0 @? "0"



  group "income groups" $ do
    let incomeGroups = History.groups incomes

    test "should find a income group" $ do
      length incomeGroups @?= 1

    test "should be united airlines" $ do
      History.name (head incomeGroups) @?= "United Airlines"

    test "should have multiple incomes" $ do
      length (History.transactions $ head incomeGroups) > 0 @? "0"


    test "biggest total group first" $ do
      let ts = [ transaction { name = "small", amount = absolute $ Money.fromFloat (-100.00) }
               , transaction { name = "small", amount = absolute $ Money.fromFloat (-100.00) }
               , transaction { name = "big",     amount = absolute $ Money.fromFloat (-150.00) }
               , transaction { name = "big",     amount = absolute $ Money.fromFloat (-150.00) }
               ]
      (map History.name $ History.groups ts) @?= ["big", "small"]


    test "biggest total should be before large single" $ do
      let ts = [ transaction { Trans.name = "regular", amount = absolute $ Money.fromFloat (-100.00) }
               , transaction { Trans.name = "regular", amount = absolute $ Money.fromFloat (-100.00) }
               , transaction { Trans.name = "big",     amount = absolute $ Money.fromFloat (-150.00) }
               ]
      (map History.name $ History.groups ts) @?= ["regular", "big"]


  group "average" $ do
    test "should have income of 500" $ do
      History.average (head $ History.groups incomes) @?= (absolute (Money.fromFloat (-500.00)))


  group "expense groups" $ do
    let expenseGroups = History.groups expenses

    test "should find multiple groups" $ do
      length expenseGroups > 1 @? "too few"

    test "should find mcdonalds" $ do
      let mcDonalds = List.find (isName "McDonald's") expenseGroups
      (History.name <$> mcDonalds) @?= Just "McDonald's"
      (History.average <$> mcDonalds) @?= Just (absolute $ Money.fromFloat 12.00)

    test "most" $ do
      let first = head expenseGroups
      History.name first @?= "SparkFun"
      History.average first @?= (absolute $ Money.fromFloat 89.40)

    test "biggest total should be before large single" $ do
      let ts = [ transaction { name = "regular", amount = absolute $ Money.fromFloat (100.00) }
               , transaction { name = "regular", amount = absolute $ Money.fromFloat (100.00) }
               , transaction { name = "big",     amount = absolute $ Money.fromFloat (150.00) }
               ]
      (map History.name $ History.groups ts) @?= ["regular", "big"]



isName x g = History.name g == x








