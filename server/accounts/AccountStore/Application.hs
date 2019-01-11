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

import AccountStore.Types
import Types.Guid (Guid)


data ApplicationStore a where
    Save      :: Application -> ApplicationStore ()
    Find      :: Guid Account  -> ApplicationStore (Maybe Application)
    All       :: ApplicationStore [Application]

    SaveResult :: Guid Account -> Result -> ApplicationStore ()
    FindResult :: Guid Account -> ApplicationStore (Maybe Result)

instance (Selda m) => Service m ApplicationStore where
    run (Save a) = saveApplication a
    run (Find i) = findApplication i
    run All      = allApplications
    run (SaveResult i r) = saveResult i r
    run (FindResult i) = findResult i



applications :: Table Application
applications = table "applications" [#accountId :- primary]

approvals :: Table AppApproval
approvals = table "applications_approvals" [#accountId :- primary]

denials :: Table AppDenial
denials = table "applications_denials" [#accountId :- primary]


saveApplication :: (Selda m) => Application -> m ()
saveApplication app = do
    insert applications [app]
    pure ()


findApplication :: (Selda m) => Guid Account -> m (Maybe Application)
findApplication i = do
    as <- query $ do
      app <- select applications
      restrict (app ! #accountId .== literal i)
      return app
    pure $ listToMaybe as


allApplications :: (Selda m) => m [Application]
allApplications =
    query $ select applications


-- Underwriting results ---------------------------------

saveResult :: (Selda m) => Guid Account -> Result -> m ()
saveResult accountId (Approved (Approval {..})) = do
    insert approvals [AppApproval {..}]
    pure ()
saveResult accountId (Denied (Denial {..})) = do
    insert denials [AppDenial {..}]
    pure ()


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

