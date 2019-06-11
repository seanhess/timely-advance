{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.History where

import qualified Data.List                          as List
import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (absolute)
import           Test.Dates                         (day)
import           Test.Evaluate.History.Transactions (allTransactions, expenses, incomes, transaction)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Evaluate.Health.Budget      (Budget (Budget))
import           Timely.Evaluate.Health.Transaction (Transaction (..))
import qualified Timely.Evaluate.Health.Transaction as Trans
import qualified Timely.Evaluate.History            as History
import           Timely.Evaluate.Schedule           (DayOfWeek (..), Schedule (..))

tests :: Tests ()
tests = do
  group "separate" testSeparate
  group "groups" testGroups
  group "spending" testSpending



testSpending :: Tests ()
testSpending = do

  let amt =  Money.fromFloat 10.00
      transStar = Trans.expense "Starbucks" amt (day "2019-01-01")
      transOther = Trans.expense "Other" amt (day "2019-01-02")
      budStar = Budget "Starbucks" (Weekly Monday) (absolute amt)
      budOther = Budget "Other" (Weekly Monday) (absolute amt)
      allSpending  = History.spending [] [transStar, transOther]
      starSpending = History.spending [budOther] [transStar, transOther]

  group "isSpending" $ do
    test "not spending if matches budget" $
      History.isSpending [budStar] transStar @?= False

    test "is spending if doesn't match" $
      History.isSpending [budStar] transOther @?= True

    test "not spending if matches budget multiple" $
      History.isSpending [budStar, budOther] transOther @?= False

    test "is spending if no budgets" $
      History.isSpending [] transStar @?= True


  group "spending" $ do
    test "should have only starbucks in spending" $ do
      length starSpending @?= 1
      (Trans.name $ head starSpending) @?= "Starbucks"

    test "should have both in all" $ do
      map Trans.name allSpending @?= ["Starbucks", "Other"]


  group "daily spending" $ do
    test "one per day" $ do
      History.dailySpending 2 allSpending @?= amt

    test "over 10 days" $ do
      History.dailySpending 10 allSpending @?= Money.fromFloat 2


testSeparate :: Tests ()
testSeparate = do
    test "should be either expenses or income" $ do
      length expenses + length incomes @?= length allTransactions

    test "should find some expenses" $ do
      length expenses > 0 @? "0"

    test "should find some income" $ do
      length incomes > 0 @? "0"


testGroups :: Tests ()
testGroups = do
  group "income groups" $ do
    let incomeGroups = History.groups incomes

    test "should find a income group" $ do
      length incomeGroups @?= 1

    test "should be united airlines" $ do
      History.name (head incomeGroups) @?= "United Airlines"

    test "should have multiple incomes" $ do
      length (History.transactions $ head incomeGroups) > 0 @? "0"


    test "biggest total group first" $ do
      let ts = [ transaction { name = "small", amount = Money.fromFloat (-100.00) }
               , transaction { name = "small", amount = Money.fromFloat (-100.00) }
               , transaction { name = "big",     amount = Money.fromFloat (-150.00) }
               , transaction { name = "big",     amount = Money.fromFloat (-150.00) }
               ]
      (map History.name $ History.groups ts) @?= ["big", "small"]


    test "biggest total should be before large single" $ do
      let ts = [ transaction { Trans.name = "regular", amount = Money.fromFloat (-100.00) }
               , transaction { Trans.name = "regular", amount = Money.fromFloat (-100.00) }
               , transaction { Trans.name = "big",     amount = Money.fromFloat (-150.00) }
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
      let ts = [ transaction { name = "regular", amount = Money.fromFloat (100.00) }
               , transaction { name = "regular", amount = Money.fromFloat (100.00) }
               , transaction { name = "big",     amount = Money.fromFloat (150.00) }
               ]
      (map History.name $ History.groups ts) @?= ["regular", "big"]



isName x g = History.name g == x








