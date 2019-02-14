{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-} {-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE DuplicateRecordFields       #-}
module Timely.AccountStore.Application
    ( initialize
    , ApplicationStore(..)
    ) where


import           Control.Monad.Selda       (Selda, insert, query, tryCreateTable, update_)
import           Control.Monad.Service     (Service (..))
import           Data.Maybe                (listToMaybe)
import           Data.Model.Guid           (Guid)
import           Database.Selda            hiding (Result, insert, query, tryCreateTable, update_)

import           Timely.AccountStore.Types
import           Timely.Underwriting.Types (Approval (..), Denial (..), Result (..))


data ApplicationStore a where
    Save      :: Application -> ApplicationStore ()
    Find      :: Guid Account -> ApplicationStore (Maybe Application)
    All       :: ApplicationStore [Application]

    SaveResult :: Guid Account -> Result -> ApplicationStore ()
    FindResult :: Guid Account -> ApplicationStore (Maybe AppResult)
    MarkResultOnboarding :: Guid Account -> Onboarding -> ApplicationStore ()

instance (Selda m) => Service m ApplicationStore where
    run (Save a)         = save a
    run (Find i)         = loadById i
    run All              = loadAll
    run (SaveResult i r) = saveResult i r
    run (FindResult i)   = findResult i
    run (MarkResultOnboarding i o) = markOnboarding i o



-- you can have more than one application per phone number
-- what if they are denied? Or cancel out for some reason?
applications :: Table Application
applications = table "applications" [#accountId :- primary]

approvals :: Table AppApproval
approvals = table "applications_approvals" [#accountId :- primary]

denials :: Table AppDenial
denials = table "applications_denials" [#accountId :- primary]


save :: (Selda m) => Application -> m ()
save app = do
    insert applications [app]
    pure ()


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


-- Underwriting results ---------------------------------

saveResult :: (Selda m) => Guid Account -> Result -> m ()
saveResult accountId (Approved (Approval {approvalAmount})) = do
    insert approvals [ AppApproval {accountId, approvalAmount, onboarding = Pending} ]
    pure ()
saveResult accountId (Denied (Denial {denial})) = do
    insert denials [AppDenial {accountId, denial}]
    pure ()


markOnboarding :: Selda m => Guid Account -> Onboarding -> m ()
markOnboarding i o = do
    update_ approvals (\a -> a ! #accountId .== literal i)
                      (\a -> a `with` [#onboarding := literal o])




-- take the first result you find, favoring denials
findResult :: Selda m => Guid Account -> m (Maybe AppResult)
findResult i = do
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
