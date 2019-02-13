{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.OnboardAccount where

import Test.Tasty.HUnit
import Test.Tasty.Monad

import           Timely.Bank as Bank
import           Timely.AccountStore.Types (BankAccount(..))
import qualified Timely.AccountStore.Types as Account
import qualified Data.Model.Guid as Guid
import qualified Data.Model.Money as Money

tests :: Tests ()
tests = do
    group "bank-accounts" testBankAccounts


testBankAccounts :: Tests ()
testBankAccounts = do
    test "should convert savings account" $ do
      let Just id = Guid.fromText "24a6b6e8-b2e2-49a0-ad7f-770c9b7a8e67"
      let ba = Account.toBankAccount id savings
      accountType ba @?= Account.Savings
      accountId ba @?= id
      balance ba @?= Money.fromFloat 210.0

    test "should convert checking account" $ do
      let Just id = Guid.fromText "24a6b6e8-b2e2-49a0-ad7f-770c9b7a8e67"
      let ba = Account.toBankAccount id checking
      accountType ba @?= Account.Checking


savings :: Account
savings = Account {account_id = Id "KEERg7Mdw9Iv7Me36RPvhw43JkXkjXTVQAbpr", balances = Balances {current = Currency 210.0, available = Just (Currency 200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "1111", name = "Plaid Saving", official_name = "Plaid Silver Standard 0.1% Interest Saving", subtype = Savings, _type = Depository}


checking :: Account
checking = Account {account_id = Id "J88kojZxdRs81ZEnmkV8Fb8NKpapGaudPgq6Z", balances = Balances {current = Currency 110.0, available = Just (Currency 100.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "0000", name = "Plaid Checking", official_name = "Plaid Gold Standard 0% Interest Checking", subtype = Checking, _type = Depository}


-- , Account {account_id = Id "KEERg7Mdw9Iv7Me36RPvhw43JkXkjXTVQAbpr", balances = Balances {current = Currency 210.0, available = Just (Currency 200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "1111", name = "Plaid Saving", official_name = "Plaid Silver Standard 0.1% Interest Saving", subtype = Savings, _type = Depository}
-- , Account {account_id = Id "kGGwgNdDXjcGLKwJd3rGSodzREMEPMcWvdK6q", balances = Balances {current = Currency 1000.0, available = Nothing, iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "2222", name = "Plaid CD", official_name = "Plaid Bronze Standard 0.2% Interest CD", subtype = SubType "cd", _type = Depository}
-- , Account {account_id = Id "qBBlnZ7D5pIXjd8KAMQXCwXZR6L6rLTd37krx", balances = Balances {current = Currency 410.0, available = Nothing, iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "3333", name = "Plaid Credit Card", official_name = "Plaid Diamond 12.5% APR Interest Credit Card", subtype = SubType "credit card", _type = Credit}
-- , Account {account_id = Id "lll78PaDg5hxRvQJg3zxcKaj9M7MR7UZxd8oM", balances = Balances {current = Currency 43200.0, available = Just (Currency 43200.0), iso_currency_code = Just (CurrencyCode "USD"), unofficial_currency_code = Nothing}, mask = "4444", name = "Plaid Money Market", official_name = "Plaid Platinum Standard 1.85% Interest Money Market", subtype = SubType "money market", _type = Depository}
-- ]
