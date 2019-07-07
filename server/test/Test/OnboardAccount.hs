{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.OnboardAccount where

import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (absolute)
import           Data.Time.Clock                    (getCurrentTime)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Accounts.Types              (BankAccount (..))
import qualified Timely.Accounts.Types              as Account
import           Timely.Accounts.Types.Application  (Onboarding (..))
import           Timely.Accounts.Types.Subscription (Level (..))
import           Timely.Actions.Transactions        as Trans (History (..))
import           Timely.Bank                        as Bank
import           Timely.Evaluate.History            (Group (..))
import           Timely.Worker.AccountOnboard       as Onboard

tests :: Tests ()
tests = do
    group "bank-accounts" testBankAccounts
    group "minimal-requirements" testMinimalRequirements


specOnboard = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" ts


testMinimalRequirements :: Tests ()
testMinimalRequirements = do
    let lowAmt = (absolute (Money.fromFloat 100))
    let highAmt = (absolute (Money.fromFloat 150))
    let low = Group "low" lowAmt lowAmt Nothing []
    let high = Group "high" highAmt highAmt Nothing []

    let billAmt = (absolute (Money.fromFloat 60))
    let payAmt = (absolute (Money.fromFloat 200))
    let bill = Group "bill" billAmt billAmt Nothing []
    let pay = Group "pay" payAmt payAmt Nothing []

    test "should fail if no income" $ do
      Onboard.checkMinimalRequirements (History [] []) @?= Left RejectedIncomeNotRegular

    test "should fail if income is low" $ do
      Onboard.checkMinimalRequirements (History [low] [high]) @?= Left RejectedIncomeLow

    test "should pass if no exp" $ do
      Onboard.checkMinimalRequirements (History [low] []) @?= Right Basic

    test "should pass if high" $ do
      Onboard.checkMinimalRequirements (History [high] [low]) @?= Right Basic

    group "multiples" $ do

      test "three bills" $ do
        Onboard.checkMinimalRequirements (History [pay] [bill, bill, bill]) @?= Right Basic

      test "four bills" $ do
        Onboard.checkMinimalRequirements (History [pay] [bill, bill, bill, bill]) @?= Left RejectedIncomeLow

    group "primary income" $ do
      test "ok if first is enough" $ do
        Onboard.checkMinimalRequirements (History [low, pay] [high]) @?= Right Basic

      test "fails not counting second" $ do
        Onboard.checkMinimalRequirements (History [low, pay] [high, bill]) @?= Left RejectedIncomeLow


testBankAccounts :: Tests ()
testBankAccounts = do
    test "should convert savings account" $ do
      now <- getCurrentTime
      let ba = Account.toBankAccount "1234" now savings
      accountType ba @?= Account.Savings
      accountId ba @?= "1234"
      balance ba @?= Money.fromFloat 210.0

    test "should convert checking account" $ do
      now <- getCurrentTime
      let ba = Account.toBankAccount "1234" now checking
      accountType ba @?= Account.Checking


savings :: Account
savings = Account {account_id = Id "KEERg7Mdw9Iv7Me36RPvhw43JkXkjXTVQAbpr", balances = Balances {current = Currency 210.0, available = Just (Currency 200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "1111", name = "Plaid Saving", official_name = Just "Plaid Silver Standard 0.1% Interest Saving", subtype = Savings, _type = Depository}


checking :: Account
checking = Account {account_id = Id "J88kojZxdRs81ZEnmkV8Fb8NKpapGaudPgq6Z", balances = Balances {current = Currency 110.0, available = Just (Currency 100.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "0000", name = "Plaid Checking", official_name = Just "Plaid Gold Standard 0% Interest Checking", subtype = Checking, _type = Depository}


-- , Account {account_id = Id "KEERg7Mdw9Iv7Me36RPvhw43JkXkjXTVQAbpr", balances = Balances {current = Currency 210.0, available = Just (Currency 200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "1111", name = "Plaid Saving", official_name = "Plaid Silver Standard 0.1% Interest Saving", subtype = Savings, _type = Depository}
-- , Account {account_id = Id "kGGwgNdDXjcGLKwJd3rGSodzREMEPMcWvdK6q", balances = Balances {current = Currency 1000.0, available = Nothing, iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "2222", name = "Plaid CD", official_name = "Plaid Bronze Standard 0.2% Interest CD", subtype = SubType "cd", _type = Depository}
-- , Account {account_id = Id "qBBlnZ7D5pIXjd8KAMQXCwXZR6L6rLTd37krx", balances = Balances {current = Currency 410.0, available = Nothing, iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "3333", name = "Plaid Credit Card", official_name = "Plaid Diamond 12.5% APR Interest Credit Card", subtype = SubType "credit card", _type = Credit}
-- , Account {account_id = Id "lll78PaDg5hxRvQJg3zxcKaj9M7MR7UZxd8oM", balances = Balances {current = Currency 43200.0, available = Just (Currency 43200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "4444", name = "Plaid Money Market", official_name = "Plaid Platinum Standard 1.85% Interest Money Market", subtype = SubType "money market", _type = Depository}
-- ]
