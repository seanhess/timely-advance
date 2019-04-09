{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.UpdateAccount where

import           Test.Dates                        (parseDay)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad

import           Data.Model.Id                     (Id (..))
import           Data.Model.Money                  as Money
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Types.Transaction
import qualified Timely.Worker.AccountUpdate       as Update

tests :: Tests ()
tests = do
    group "update-transactions" testUpdateTransactions


testUpdateTransactions :: Tests ()
testUpdateTransactions = do
  group "new transactions" $ do
    test "accounts empty" $
      Update.transNew [t1] [] @?= [t1]

    test "banks empty" $
      Update.transNew [] [t1, t2, t3] @?= []

    test "removes existing items" $
      Update.transNew [t1, t2, t3] [t2] @?= [t1, t3]

  group "all transactions" $ do
    test "accounts empty" $
      Update.transAll [t1] [] @?= [t1]

    test "banks empty" $
      Update.transAll [] [t1, t2] @?= [t1, t2]

    test "all items" $
      Update.transAll [t1, t2, t3] [t2, t1, t4] @?= [t1, t2, t3, t4]







t1 = transaction { transactionId = Id "1" }
t2 = transaction { transactionId = Id "2" }
t3 = transaction { transactionId = Id "3" }
t4 = transaction { transactionId = Id "4" }
t5 = transaction { transactionId = Id "5" }

transaction =
    TransactionRow {transactionId = Id "5gDKvpZmyWFEaVJMVqaXIqrWQKAkKdhZv719B", accountId = "acc-DgKodQpnuLMOgkRgoR4d", date = parseDay "2019-03-19", category = Category "Food and Drink | Restaurants | Coffee Shop", pending = False, amount = Money 433, name = "Starbucks"}
