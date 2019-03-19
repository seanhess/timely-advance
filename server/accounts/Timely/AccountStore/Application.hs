{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
module Timely.AccountStore.Application where


import           Control.Effects           (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda       (Selda, insert, query, tryCreateTable, update_)
import           Data.Maybe                (listToMaybe, catMaybes)
import           Data.Model.Guid           (Guid)
import           Data.Model.Id             (Id(..))
import qualified Data.Time.Clock           as Time
import           Database.Selda            hiding (Result, insert, query, tryCreateTable, update_)

import           Timely.AccountStore.Types
import qualified Timely.Bank               as Bank
import           Timely.Underwriting.Types (Approval (..), Denial (..), Result (..))


data Applications m = ApplicationsMethods
  { _save                 :: Application -> m ()
  , _find                 :: Guid Account -> m (Maybe Application)
  , _all                  :: m [Application]
  , _check                :: m [Application]
  , _saveResult           :: Guid Account -> Result -> m ()
  , _findResult           :: Guid Account -> m (Maybe AppResult)
  , _markResultOnboarding :: Guid Account -> Onboarding -> m ()
  , _saveBank             :: Guid Account -> Id Bank.Item -> m (Id AppBank)
  , _saveTransactions     :: Id Bank.Item -> Int -> m ()
  , _findTransactions     :: Id AppBank -> m (Maybe Int)
  } deriving (Generic)

instance Effect Applications


save :: MonadEffect Applications m => Application -> m ()
find :: MonadEffect Applications m => Guid Account -> m (Maybe Application)
all :: MonadEffect Applications m => m [Application]
check :: MonadEffect Applications m => m [Application]
saveResult :: MonadEffect Applications m => Guid Account -> Result -> m ()
findResult :: MonadEffect Applications m => Guid Account -> m (Maybe AppResult)
markResultOnboarding :: MonadEffect Applications m => Guid Account -> Onboarding -> m ()
saveBank :: MonadEffect Applications m => Guid Account -> Id Bank.Item -> m (Id AppBank)
saveTransactions :: MonadEffect Applications m => Id Bank.Item -> Int -> m ()
findTransactions :: MonadEffect Applications m => Id AppBank -> m (Maybe Int)
ApplicationsMethods save find all check saveResult findResult markResultOnboarding saveBank saveTransactions findTransactions = effect



implementIO :: Selda m => RuntimeImplemented Applications m a -> m a
implementIO = implement $
  ApplicationsMethods
    saveApp
    loadById
    checkHealth
    loadAll
    saveAppResult
    findAppResult
    markAppOnboarding
    saveAppBank
    saveAppTransactions
    findAppTransactions





-- you can have more than one application per phone number
-- what if they are denied? Or cancel out for some reason?
applications :: Table Application
applications = table "applications" [#accountId :- primary]

approvals :: Table AppApproval
approvals = table "applications_approvals" [#accountId :- primary]

denials :: Table AppDenial
denials = table "applications_denials" [#accountId :- primary]

banks :: Table AppBank
banks = table "applications_banks" [#bankItemId :- primary]


saveApp :: (Selda m) => Application -> m ()
saveApp app = do
    insert applications [app]
    pure ()


saveAppBank :: Selda m => Guid Account -> Id Bank.Item -> m (Id AppBank)
saveAppBank accountId bankItemId = do
    insert banks [AppBank accountId bankItemId Nothing]
    let Id i = bankItemId
    pure $ Id i

saveAppTransactions :: Selda m => Id Bank.Item -> Int -> m ()
saveAppTransactions i n = do
    update_ banks
      (\a -> a ! #bankItemId .== literal i)
      (\a -> a `with` [#transactions := literal (Just n)])


findAppTransactions :: (Selda m) => Id AppBank -> m (Maybe Int)
findAppTransactions (Id i) = do
    -- bs [Maybe Int]
    bs <- query $ do
      b <- select banks
      restrict (b ! #bankItemId .== literal (Id i))
      return $ b ! #transactions
    pure $ listToMaybe $ catMaybes $ bs




loadById :: (Selda m) => Guid Account -> m (Maybe Application)
loadById i = do
    as <- query $ do
      app <- select applications
      restrict (app ! #accountId .== literal i)
      return app
    pure $ listToMaybe as


loadAll :: (Selda m) => m [Application]
loadAll =
    query $ select applications


checkHealth :: Selda m => m [Application]
checkHealth = query $ limit 0 1 $ select applications




-- Underwriting results ---------------------------------

saveAppResult :: (Selda m) => Guid Account -> Result -> m ()
saveAppResult accountId (Approved (Approval {approvalAmount})) = do
    now <- liftIO $ Time.getCurrentTime
    insert approvals [ AppApproval {accountId, approvalAmount, created = now} ]
    pure ()
saveAppResult accountId (Denied (Denial {denial})) = do
    now <- liftIO $ Time.getCurrentTime
    insert denials [AppDenial {accountId, denial, created = now}]
    pure ()


markAppOnboarding :: Selda m => Guid Account -> Onboarding -> m ()
markAppOnboarding i o = do
    update_ applications
      (\a -> a ! #accountId .== literal i)
      (\a -> a `with` [#onboarding := literal o])




-- take the first result you find, favoring denials
findAppResult :: Selda m => Guid Account -> m (Maybe AppResult)
findAppResult i = do
    ds <- query $ do
      d <- select denials
      restrict (d ! #accountId .== literal i)
      pure d
    as <- query $ do
      a <- select approvals
      restrict (a ! #accountId .== literal i)
      pure a
    pure $ listToMaybe $ map fromAppDenial ds ++ map fromAppApproval as
  where
    fromAppDenial appDenial = AppResultDenial appDenial
    fromAppApproval appApproval = AppResultApproval appApproval






initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable applications
    tryCreateTable approvals
    tryCreateTable denials
    tryCreateTable banks
