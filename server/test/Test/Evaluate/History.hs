{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.History where

import qualified Data.List                         as List
import           Data.Model.Id                     (Id (..))
import           Data.Model.Money                  (Money (..))
import qualified Data.Model.Money                  as Money
import           Data.Number.Abs                   (absolute)
import           Test.Dates                        (parseDay)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Accounts                   (Category (..), Transaction (..))
import qualified Timely.Accounts.Types.Transaction as Trans
import           Timely.Evaluate.History           as History hiding (expenses, income)

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
      length (transactions $ head incomeGroups) > 0 @? "0"


    test "biggest total group first" $ do
      let ts = [ transaction { Trans.name = "small", amount = Money.fromFloat (-100.00) }
               , transaction { Trans.name = "small", amount = Money.fromFloat (-100.00) }
               , transaction { Trans.name = "big",     amount = Money.fromFloat (-150.00) }
               , transaction { Trans.name = "big",     amount = Money.fromFloat (-150.00) }
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
      average (head $ groups incomes) @?= (absolute (Money.fromFloat (-500.00)))


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
      let ts = [ transaction { Trans.name = "regular", amount = Money.fromFloat (100.00) }
               , transaction { Trans.name = "regular", amount = Money.fromFloat (100.00) }
               , transaction { Trans.name = "big",     amount = Money.fromFloat (150.00) }
               ]
      (map History.name $ History.groups ts) @?= ["regular", "big"]






expenses = List.filter History.isExpense allTransactions
incomes  = List.filter History.isIncome allTransactions
isName x g = History.name g == x




transaction =
    Transaction {transactionId = Id "5gDKvpZmyWFEaVJMVqaXIqrWQKAkKdhZv719B", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-03-19", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}






allTransactions =
  [ Transaction {transactionId = Id "5gDKvpZmyWFEaVJMVqaXIqrWQKAkKdhZv719B", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-03-19", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "J5ZgMLPDB3S5V68N6GV1sARqLZ54ZjSdAgD9R", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-03-18", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "kqdZ7E1WzVi1rmGkmyrLhQkLAw95wGhW9dlXN", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-03-05", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "lKaMeELnBwSlzqxbqXzRtlJzmw43w5CZ4dVkl", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-02-20", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "qx7gLwEy4zsnQbXPb1Qjhe7vpVEKVzCdE7zZZ", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-02-18", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "K5M81kjD3xS5PkvNk4P7synl5a6Za8CVyAxLa", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-02-17", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "rde8pElnNBI3l9yP9wlXUbDWJPQRPetlQ6eng", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-02-17", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "J5ZgMLPDB3S5V68N6GV1sARZpzelABcd4d6rL", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-02-16", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "5gDKvpZmyWFEaVJMVqaXIqrKGzE6qmIZwZKP1", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-02-03", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "wjLnWEBmQ4SVWRxMReWbuKoDg8QkK6crNrZ7X", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-01-21", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "V5nRb48DkMS5KawpalK6sXNGlg4pXbcWmWg8g", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-01-19", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "X5mXyPlWKLSlXQVJQ1XPt6KPZ9xD6Ncd6dKWk", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-01-18", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "D5NZP73DylS58dXvdQ87svdWG9Z4vBuvwvWPR", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-01-18", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "68VrRlMEyAs3jJX9JKjRUbQDgmBVbACg9gDoP", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-01-17", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "R5EmW3LDj7S5bV84VMbes5GERA695WuRlRpvA", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-01-04", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "vP1GEXzneas4zlAJl8zrTb8z7vKRbDCWzW63R", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-12-22", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "9Al3qrpEybUaj8R58BjGIAXVnv9JAwcRGRxWQ", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-12-20", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "mk41Qypn3liqQLVxLgQ4H6Jmox3p65cLmLyvn", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-12-19", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "yeomGqxnJPIjx6WR6oxDfPj37q6NPQty3y6qe", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-12-19", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "bJ5dBE4WPbh4xoWkoDxrTxMe5JPExDfVxVb4d", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-12-18", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "nzNLZEonxqIK31W91w3QSGQe6gZVGBs6e6lyv", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-12-05", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "G57LeZaDKrS5o6a163o8s8kMAvQ98VC1B1Mem", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-11-22", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "ARd7aNrEyDSZqwEpwjqmI8dlWVLM84C1j1rXg", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-11-20", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "E5PMjexDlqS5zgJogrzLsyQmWELXyVsXRXxLW", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-11-19", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "W5b6mNEDq1S5rzJDzwrbsBeVGlyRBDFl9lZGL", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-11-19", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "8k5BlKLEynidmVZkVzmMsdNknpEadlFw1w3yk", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-11-18", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "g5zj49eWlRSepqGMqJpdhVQ9nz1wVEtgVg8Xe", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-11-05", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "oBPpMEgndKTrV5LP5WV4SbVM6glKbJCRMRq3x", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-10-23", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "p3jo1w8nkrfzQJbMJaQ4UxBrVg4jx9fLrLoxm", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-10-21", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "11vXbZjE7osqBglMgABvHXEmWRN8X1c5K5jwV", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-10-20", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "L51ELl7DxdS5PgZNgoPwskRAKlLXk9HPQPjJd", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-10-20", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "M5LWpqeDdaS5PJk4JRPZswjJpQ7LwDS9l96om", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-10-19", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "Z59lv7KWzoSK3yEayq3XSR1xzw63RXcgRgqjP", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-10-06", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "Q58bEB1DPjS5NQgEQpNJs94ax6g19btpMpmdz", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-09-23", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "e5KNdEjWe9Sj13nM3m1Afd6G5VJPdjFLdL9wV", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-09-21", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "jlxQjEMnrDHAVMdxMXVNsy8MAJa6y3s1M1W3v", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-09-20", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "7Ewz6q1DyGsEGQ5PQrGRI9kwB3qM9vtglgJGM", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-09-20", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "P5gn8zMDNPS5NEedE7NXslrBpL61lVu7k764o", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-09-19", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "N5pkoKQDa4S5NBnLBqNVs7gMpVBe76UWjW61W", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-09-06", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "4oXq9LQ5yESEMWG6WlM7ImybvVpQmnIdrdbxg", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-08-24", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "a5wxzpabNnSvyNlEN7yoIoaADRK7obC7a759p", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-08-22", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "x7pbAl3nkNfP3qxMqE35tMp76GoLMQfn7n4EZ", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-08-21", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "d5q4xEP67ySWD6E56RDbS9plozdW9atZMZE3d", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-08-21", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "3p6ly1eAk8sRyEGaEQynt3ylVDpr3dhqrqlax", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-08-20", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "B6ae4PzbyVHElZA6Z7l1IBWXEo5yBwFwWwMGE", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-08-07", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "zQgykKdXojHkRDzJDKRgseyPjM6Qeouo7oNGa", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-07-25", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "rde8pElnNBI3l9yP9wlXUbDPzAdrbqClPlJvP", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-07-23", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "qx7gLwEy4zsnQbXPb1Qjhe7V6Q3PequdmdrqE", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-07-22", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "K5M81kjD3xS5PkvNk4P7synakNqpyWsVZVpv9", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-07-22", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "lKaMeELnBwSlzqxbqXzRtlJwMeLglruZwZovy", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-07-21", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "kqdZ7E1WzVi1rmGkmyrLhQkwEgpDQbTWQW6bV", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-07-08", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "J5ZgMLPDB3S5V68N6GV1sARZpzelABcd4d6LL", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-06-25", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "5gDKvpZmyWFEaVJMVqaXIqrKGzE6qmIZwZKD1", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-06-23", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "V5nRb48DkMS5KawpalK6sXNGlg4pXbcWmWgJg", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-06-22", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "wjLnWEBmQ4SVWRxMReWbuKoDg8QkK6crNrZ6X", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-06-22", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "D5NZP73DylS58dXvdQ87svdWG9Z4vBuvwvW8R", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-06-21", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "X5mXyPlWKLSlXQVJQ1XPt6KPZ9xD6Ncd6dKRk", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-06-08", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "68VrRlMEyAs3jJX9JKjRUbQDgmBVbACg9gDRP", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-05-26", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "R5EmW3LDj7S5bV84VMbes5GERA695WuRlRpQA", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-05-24", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "9Al3qrpEybUaj8R58BjGIAXVnv9JAwcRGRxmQ", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-05-23", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "vP1GEXzneas4zlAJl8zrTb8z7vKRbDCWzW6DR", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-05-23", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "yeomGqxnJPIjx6WR6oxDfPj37q6NPQty3y6Qe", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-05-22", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "mk41Qypn3liqQLVxLgQ4H6Jmox3p65cLmLy5n", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-05-09", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "bJ5dBE4WPbh4xoWkoDxrTxMe5JPExDfVxVbwd", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-04-26", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "nzNLZEonxqIK31W91w3QSGQe6gZVGBs6e6lBv", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-04-24", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "ARd7aNrEyDSZqwEpwjqmI8dlWVLM84C1j1rvg", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-04-23", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "G57LeZaDKrS5o6a163o8s8kMAvQ98VC1B1M4m", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-04-23", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "W5b6mNEDq1S5rzJDzwrbsBeVGlyRBDFl9lZkL", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-04-22", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  , Transaction {transactionId = Id "E5PMjexDlqS5zgJogrzLsyQmWELXyVsXRXxqW", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-04-09", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 633, Trans.name = "Uber 072515 SF**POOL**"}
  , Transaction {transactionId = Id "8k5BlKLEynidmVZkVzmMsdNknpEadlFw1w3bk", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-03-27", category = Category "Travel | Car Service | Ride Share", pending = False, amount = Money 540, Trans.name = "Uber 063015 SF**POOL**"}
  , Transaction {transactionId = Id "g5zj49eWlRSepqGMqJpdhVQ9nz1wVEtgVg8xe", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-03-25", category = Category "Travel | Airlines and Aviation Services", pending = False, amount = Money (-50000), Trans.name = "United Airlines"}
  , Transaction {transactionId = Id "p3jo1w8nkrfzQJbMJaQ4UxBrVg4jx9fLrLoZm", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-03-24", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 1200, Trans.name = "McDonald's"}
  , Transaction {transactionId = Id "oBPpMEgndKTrV5LP5WV4SbVM6glKbJCRMRqdx", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-03-24", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, Trans.name = "Starbucks"}
  , Transaction {transactionId = Id "L51ELl7DxdS5PgZNgoPwskRAKlLXk9HPQPjXd", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2018-03-23", category = Category "Food and Drink | Restaurants", pending = False, amount = Money 8940, Trans.name = "SparkFun"}
  ]
