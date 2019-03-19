{-# LANGUAGE OverloadedStrings #-}
module Test.Bank where

import           Control.Effects  (implement)
import           Data.Function    ((&))
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Bank      as Bank

tests :: Tests ()
tests = do
    group "transactions" testTransactions



testTransactions :: Tests ()
testTransactions = do
    group "looping range" $ do
      test "should return short list" $ do
        ts <- Bank.loadTransactionsDays undefined undefined undefined undefined & implement (emptyMethods {_loadTransactions = mockLoad 10})
        length ts @?= 10

      test "should return long list" $ do
        ts <- Bank.loadTransactionsDays undefined undefined undefined undefined & implement (emptyMethods {_loadTransactions = mockLoad 750})
        length ts @?= 750

      test "should only call once if empty" $ do
        let mockLoad' _ _ options = do
              offset options @?= 0
              pure []

        ts <- Bank.loadTransactionsDays undefined undefined undefined undefined & implement (emptyMethods {_loadTransactions = mockLoad'})
        ts @?= []

  where
    mockLoad num _ _ options = do
      let results = replicate num undefined
      pure $ take (count options) $ drop (offset options) results


emptyMethods :: Banks IO
emptyMethods = BanksMethods undefined undefined undefined undefined undefined
