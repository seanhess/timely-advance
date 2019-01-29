{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

import           Control.Exception             (Exception)
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.Service         (Service (run))
import qualified Data.List                     as List
import qualified Network.AMQP.Worker           as Worker

import           Timely.AccountStore.Account   (AccountStore)
import qualified Timely.AccountStore.Account   as AccountStore
import           Timely.AccountStore.Types     (Account (bankToken, credit), BankAccount (accountType, balance),
                                                BankAccountType (Checking), toBankAccount)
import           Timely.Advances               (Advances)
import qualified Timely.Advances               as Advances
import           Timely.Bank                   (Access, Banks, Token)
import qualified Timely.Bank                   as Bank
import           Timely.Evaluate.AccountHealth (Health (..))
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import           Timely.Events                 as Events
import           Timely.Types.Guid             (Guid)
import           Timely.Types.Private          (Private (..))


queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.transactionsNew "app.account.update"


-- it's not evaluate, its: what do we do on a new transaction
-- AccountAction
-- UpdateAccount
-- Yeah, this is idempotent. You can run it over and over and it won't fuck up. but we happen to run it on new transactions, every day, etc
-- Also, I don't think it should actually transfer any money. It feels like that's more formal.

handler
  :: ( Service m Banks
     , Service m AccountStore
     , Service m Advances
     , MonadError EvaluateError m
     )
  => Guid Account -> m ()
handler accountId = do

    account  <- run (AccountStore.Find accountId)
                  >>= require MissingAccount

    checking <- updateBankBalances accountId (private $ bankToken account)
                  >>= require MissingChecking

    advances <- run (Advances.FindActive accountId)

    let health   = AccountHealth.analyze $ AccountHealth.Info (credit account) (balance checking) advances

    handleHealth health

  where

    require :: MonadError EvaluateError m => (Guid Account -> EvaluateError) -> (Maybe a) -> m a
    require err Nothing = throwError (err accountId)
    require _ (Just a)  = pure a




-- | updates the bank accounts and returns the checking account
updateBankBalances
    :: ( Service m Banks
       , Service m AccountStore
       )
    => Guid Account -> Token Access -> m (Maybe BankAccount)
updateBankBalances accountId token = do
    banks <- run $ Bank.LoadAccounts token
    let accounts = List.map (toBankAccount accountId) banks
    run $ AccountStore.SetBankAccounts accountId accounts
    pure $ List.find isChecking accounts
  where
    isChecking :: BankAccount -> Bool
    isChecking acc = accountType acc == Checking


-- TODO logging! so we can see the results of nothing happened / maxed credit

handleHealth :: Monad m => Health -> m ()
handleHealth Ok = pure ()
handleHealth (Maxed _ _) = pure ()
    -- TODO what do we want to do if their credit is maxed?
    -- maybe it's not my job to handle this..

handleHealth (Needs _) = do
    -- TODO store advances
    -- TODO schedule advance
    -- TODO schedule payment

    pure ()


-- hmmmmmmmm..... so, I decide they need an advance, but they don't necessarily have enough credit. What should I do?
-- it's simplier if I have this guy issue the advance also
-- why not?
-- handle it in code, not in asynchronous messiness
-- "advances" that are issued are VERY different than ones that should be issued
-- yeah that's a health result, no?
-- account needs money
-- account needs money but is maxed
-- account is ok


-- they can't really have more than one state of health


-- what about if we say "they need an advance!", but it hasn't been issued yet. It hasn't hit their account yet. So they still need money, but they don't have it
-- how do I handle this?
-- There's a pending state: they need money, but they don't have it yet
---- We sent them an advance. Are they still in jeopardy?
---- pending advances... hmm.... how should we tell if they need them, or whatever?

-- TEST what if they have two transactions right after each other? They might need more money than we expect. We begin an advance right away, but then along comes another transaction, and they're going to need even MORE money. No, only if we miscalculated. Because transactions are normal. It's not like we expect them to not spend money. But sometimes they do something unexpected. We miscalculated. We should CORRECT the advance. But it's pending. So do another one


-- Identify pending advances? Some way to link them up with transaction ids. I'm not sure how they'll even show up. It would be better if we don't have to rely on that 
-- Look, we know we sent them money, but we don't know if it's *arrived* yet
-- We don't want to send more than one a day, that's for sure
-- we might want to say: "They need $X dollars"
-- but at what point do we act on that?





data EvaluateError
    = MissingAccount  (Guid Account)
    | MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception EvaluateError
