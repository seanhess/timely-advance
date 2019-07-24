{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.OnboardAccount where

import           Data.Model.Id                      (Id (..))
import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (absolute)
import           Data.Time.Clock                    (getCurrentTime)
import           Network.Plaid.Types                as Plaid (Account (..), AccountSubType (..), AccountType (..), Balances (..), CurrencyCode (..))
import           Test.Dates                         (day)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Accounts.Types              (BankAccount (..), BankAccountType (..))
import qualified Timely.Accounts.Types              as Account
import           Timely.Accounts.Types.Application  (Onboarding (..), Rejected (..))
import           Timely.Accounts.Types.Subscription (Level (..))
import           Timely.Actions.Transactions        as Trans (History (..))
import           Timely.Bank                        as Bank (Currency (..))
import qualified Timely.Bank.Types                  as Bank
import           Timely.Evaluate.Health.Transaction as Transaction (any)
import           Timely.Evaluate.History            (Group (..))
import           Timely.Evaluate.Schedule           as Schedule (DayOfWeek(..), untilLE, Schedule(..), DayOfMonth(DayOfMonth))
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
    let trans d = Transaction.any "name" (Money.fromFloat 0) d
    let lowAmt = (absolute (Money.fromFloat 100))
    let highAmt = (absolute (Money.fromFloat 150))
    let history = [trans (day "2019-01-01"), trans (day "2019-02-01"), trans (day "2019-03-01")]
    let low = Group "low" lowAmt lowAmt Nothing history
    let high = Group "high" highAmt highAmt Nothing history

    let billAmt = (absolute (Money.fromFloat 60))
    let payAmt = (absolute (Money.fromFloat 200))
    let bill = Group "bill" billAmt billAmt Nothing history
    let pay = Group "pay" payAmt payAmt Nothing history

    test "should fail if no income" $ do
      Onboard.checkMinimalRequirements (History [] []) @?= Left (Rejected IncomeNotRegular)

    test "should fail if income is low" $ do
      Onboard.checkMinimalRequirements (History [low] [high]) @?= Left (Rejected IncomeLow)

    test "should pass if no exp" $ do
      Onboard.checkMinimalRequirements (History [low] []) @?= Right Basic

    test "should pass if high" $ do
      Onboard.checkMinimalRequirements (History [high] [low]) @?= Right Basic

    group "weekly income" $ do
      test "not enough for single bill" $ do
        let low'  = low { schedule = Just $ Weekly Monday }
        let high' = high { schedule = Just $ Weekly Monday }
        Onboard.checkMinimalRequirements (History [low'] [high']) @?= Left (Rejected IncomeLow)


      test "work up to single bill" $ do
        let low'  = low { schedule = Just $ Weekly Monday }
        let high' = high { schedule = Just $ Monthly (DayOfMonth 1) }
        Onboard.checkMinimalRequirements (History [low'] [high']) @?= Right Basic

      test "multiple bills, low" $ do
        let low'  = low { schedule = Just $ Weekly Monday }
        let high' = high { schedule = Just $ Monthly (DayOfMonth 1) }
        Onboard.checkMinimalRequirements (History [low'] [high', high', high', high', high']) @?= Left (Rejected IncomeLow)

      test "multiple bills, enough" $ do
        let low'  = low { schedule = Just $ Weekly Monday }
        let high' = high { schedule = Just $ Monthly (DayOfMonth 1) }
        Onboard.checkMinimalRequirements (History [low'] [high', high', high', high']) @?= Left (Rejected IncomeLow)

    group "multiples" $ do

      test "three bills" $ do
        Onboard.checkMinimalRequirements (History [pay] [bill, bill, bill]) @?= Right Basic

      test "four bills" $ do
        Onboard.checkMinimalRequirements (History [pay] [bill, bill, bill, bill]) @?= Left (Rejected IncomeLow)

    group "primary income" $ do
      test "ok if first is enough" $ do
        Onboard.checkMinimalRequirements (History [low, pay] [high]) @?= Right Basic

      test "fails not counting second" $ do
        Onboard.checkMinimalRequirements (History [low, pay] [high, bill]) @?= Left (Rejected IncomeLow)

    group "minimum two months" $ do
      test "exactly three months is long enough" $ do
        let pay3Months = Group "pay" payAmt payAmt Nothing [trans (day "2019-01-01"), trans (day "2019-02-01"), trans (day "2019-03-01")]
        Onboard.checkMinimalRequirements (History [pay3Months] []) @?= Right Basic

      test "if income history is too short" $ do
        let pay2Months = Group "pay" payAmt payAmt Nothing [trans (day "2019-01-01"), trans (day "2019-02-01")]
        Onboard.checkMinimalRequirements (History [pay2Months] []) @?= Left (Rejected IncomeTooShort)

      test "10 weeks is enough" $ do
        let history = Schedule.untilLE (day "2019-03-12") (Weekly Tuesday) (day "2019-01-01")
        length history @?= 10
        let pay' = Group "pay" payAmt payAmt Nothing (map trans history)
        Onboard.checkMinimalRequirements (History [pay'] []) @?= Right Basic



testBankAccounts :: Tests ()
testBankAccounts = do
    test "should convert savings account" $ do
      now <- getCurrentTime
      let ba = Account.toBankAccount "1234" now $ Bank.toAccount savings
      accountType ba @?= BankAccountType Account.Savings
      accountId ba @?= "1234"
      balance ba @?= Money.fromFloat 210.0

    test "should convert checking account" $ do
      now <- getCurrentTime
      let ba = Account.toBankAccount "1234" now $ Bank.toAccount checking
      accountType ba @?= BankAccountType Account.Checking


savings :: Plaid.Account
savings = Plaid.Account {account_id = Id "KEERg7Mdw9Iv7Me36RPvhw43JkXkjXTVQAbpr", balances = Plaid.Balances {current = Currency 210.0, available = Just (Currency 200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "1111", name = "Plaid Saving", official_name = Just "Plaid Silver Standard 0.1% Interest Saving", subtype = Plaid.Savings, _type = Plaid.Depository}


checking :: Plaid.Account
checking = Plaid.Account {account_id = Id "J88kojZxdRs81ZEnmkV8Fb8NKpapGaudPgq6Z", balances = Balances {current = Currency 110.0, available = Just (Currency 100.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "0000", name = "Plaid Checking", official_name = Just "Plaid Gold Standard 0% Interest Checking", subtype = Plaid.Checking, _type = Plaid.Depository}


-- , Account {account_id = Id "KEERg7Mdw9Iv7Me36RPvhw43JkXkjXTVQAbpr", balances = Balances {current = Currency 210.0, available = Just (Currency 200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "1111", name = "Plaid Saving", official_name = "Plaid Silver Standard 0.1% Interest Saving", subtype = Savings, _type = Depository}
-- , Account {account_id = Id "kGGwgNdDXjcGLKwJd3rGSodzREMEPMcWvdK6q", balances = Balances {current = Currency 1000.0, available = Nothing, iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "2222", name = "Plaid CD", official_name = "Plaid Bronze Standard 0.2% Interest CD", subtype = SubType "cd", _type = Depository}
-- , Account {account_id = Id "qBBlnZ7D5pIXjd8KAMQXCwXZR6L6rLTd37krx", balances = Balances {current = Currency 410.0, available = Nothing, iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "3333", name = "Plaid Credit Card", official_name = "Plaid Diamond 12.5% APR Interest Credit Card", subtype = SubType "credit card", _type = Credit}
-- , Account {account_id = Id "lll78PaDg5hxRvQJg3zxcKaj9M7MR7UZxd8oM", balances = Balances {current = Currency 43200.0, available = Just (Currency 43200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "4444", name = "Plaid Money Market", official_name = "Plaid Platinum Standard 1.85% Interest Money Market", subtype = SubType "money market", _type = Depository}
-- ]
