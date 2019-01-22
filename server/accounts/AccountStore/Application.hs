{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module AccountStore.Application
    ( initialize
    , ApplicationStore(..)
    ) where


import Underwriting.Types (Result(..), Approval(..), Denial(..))
import Control.Monad.Selda (Selda, query, insert, tryCreateTable)
import Database.Selda hiding (query, insert, tryCreateTable, Result)
import Data.Maybe (listToMaybe)
import Control.Monad.Service (Service(..))

import Auth (Phone)
import AccountStore.Types
import Types.Guid (Guid)


data ApplicationStore a where
    Save      :: Application -> ApplicationStore ()
    FindByPhone :: Phone -> ApplicationStore (Maybe Application)
    All       :: ApplicationStore [Application]

    SaveResult :: Guid Account -> Result -> ApplicationStore ()
    FindResult :: Guid Account -> ApplicationStore (Maybe Result)
    FindResultByPhone :: Phone -> ApplicationStore (Maybe Result)

instance (Selda m) => Service m ApplicationStore where
    run (Save a) = save a
    run (FindByPhone i) = loadByPhone i
    run All      = loadAll
    run (SaveResult i r) = saveResult i r
    run (FindResult i) = findResult i
    run (FindResultByPhone p) = findResultByPhone p



applications :: Table Application
applications = table "applications" [#accountId :- primary, #phone :- index, #phone :- unique]

approvals :: Table AppApproval
approvals = table "applications_approvals" [#accountId :- primary]

denials :: Table AppDenial
denials = table "applications_denials" [#accountId :- primary]


save :: (Selda m) => Application -> m ()
save app = do
    insert applications [app]
    pure ()


loadByPhone :: (Selda m) => Phone -> m (Maybe Application)
loadByPhone p = do
    as <- query $ do
      app <- select applications
      restrict (app ! #phone .== literal p)
      return app
    pure $ listToMaybe as


loadAll :: (Selda m) => m [Application]
loadAll =
    query $ select applications


-- Underwriting results ---------------------------------

saveResult :: (Selda m) => Guid Account -> Result -> m ()
saveResult accountId (Approved (Approval {..})) = do
    insert approvals [AppApproval {..}]
    pure ()
saveResult accountId (Denied (Denial {..})) = do
    insert denials [AppDenial {..}]
    pure ()



findResultByPhone :: Selda m => Phone -> m (Maybe Result)
findResultByPhone p = do
    ma <- loadByPhone p
    case ma of
      Nothing -> pure Nothing
      Just a -> findResult $ accountId (a :: Application)



-- take the first result you find, favoring denials
findResult :: Selda m => Guid Account -> m (Maybe Result)
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
    fromAppDenial (AppDenial {..}) = Denied $ Denial {..}
    fromAppApproval (AppApproval {..}) = Approved $ Approval {..}






initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable applications
    tryCreateTable approvals
    tryCreateTable denials

