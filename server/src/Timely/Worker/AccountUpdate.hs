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
import           Timely.AccountStore.Types     (Account (bankToken), BankAccount (balance), isChecking, toBankAccount)
import           Timely.Advances               (Advances)
import qualified Timely.Advances               as Advances
import           Timely.Bank                   (Access, Banks, Token)
import qualified Timely.Bank                   as Bank
-- import           Timely.Evaluate.Types (Projection (..))
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import           Timely.Events                 as Events
import           Timely.Types.Guid             (Guid)
import           Timely.Types.Private          (Private (..))


queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.transactionsNew "app.account.update"



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

    let projection   = AccountHealth.analyze (balance checking) advances

    run $ AccountStore.SetHealth accountId projection


  where

    require :: MonadError err m => (Guid Account -> err) -> (Maybe a) -> m a
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


-- TODO logging! so we can see the results of nothing happened / maxed credit
-- just save the health!

-- handleHealth :: Monad m => Health -> m ()
-- handleHealth Ok = pure ()
-- handleHealth (Maxed _ _) = pure ()
--     -- TODO what do we want to do if their credit is maxed?
--     -- maybe it's not my job to handle this..

-- handleHealth (Needs _) = do
--     -- TODO store advances
--     -- TODO schedule advance
--     -- TODO schedule payment

--     pure ()


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


-- ASSUME that we need to support batch advances. We might calculate their account health multiple times in 30 minutes. We want to delay X minutes, then figure out which account health to use (the one that says they need the most). No, the most recent one. Each time we recalculate the account health, delay this timer for another 30 minutes. Like amazon's shipping system.

-- Or, maybe this runs every hour for every account. In which case we want to issue the advance immediately.

-- And then, we go ahead and issue the advance: Marked as SENT. We don't do another advance until.... what.... we find a credit matching the amount in their account. It's been RECEIVED.

-- We obviously can't immediately transfer them money here.
-- But MAYBE we should Authoritatively say that we are going to send them money
-- Like, does the app show them?

-- Advance (pending)
-- Advances

-- TODO need a way to know if advances are pending or not. Do they have the money, or have we just SENT them money?

-- Is there a way to simplify this?
---- run this once a day
---- if they need money, send it immediately
---- day 2, they still neec money. We have one advance out, it hasn't arrived yet. They're in the same situation as before (roughly the same account health)
---- day 3, they still need the same money
---- day 4, the money arrives. Now they're doing good


-- but the account looks the same on day 2
-- no, their "balance" is BALANCE + PENDING ADVANCE
-- the probelm is a real advance becomes part of their advance. It's future money
-- we can calculate it if we know it's still pending


-- ASSUME: that we know if an advance is pending.. WE neet to mark them as pending / active / etc at some point (when we analyze their transactions, aka here)


-- ASSUME: daily... or on an interval we KNOW they will resolve. 2-3 days? erm... WE could scan in the middle of the night


-- DAY 1: they need money
-- DAY 2: (A) they still need money, it's on the way. pending advance. So they're healthy!
-- DAY 2: (B) things are worse, they need MORE money. We can send them another advance?
-- DAY 3: Advance arrives, they're healthy


-- It sounds like I should just store the account health here, and make the advance issuing system be completely different. Maybe it should scan the system for unhealthy accounts and then issue advances.

-- Yeah maybe this updates the account to say.. It's in jeopardy! It doesn't issue advances at all. Then this can run as much as it wants, and take everything into account. All accounts will be up-to-date whenever we decide to run the advancer thing.

data EvaluateError
    = MissingAccount  (Guid Account)
    | MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception EvaluateError
