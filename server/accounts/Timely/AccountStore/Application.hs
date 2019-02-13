{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Timely.AccountStore.Application
    ( initialize
    , ApplicationStore(..)
    ) where


import           Control.Monad.Selda       (Selda, insert, query, tryCreateTable)
import           Control.Monad.Service     (Service (..))
import           Data.Maybe                (listToMaybe)
import           Data.Model.Guid           (Guid)
import           Database.Selda            hiding (Result, insert, query, tryCreateTable)

import           Timely.AccountStore.Types
import           Timely.Underwriting.Types (Approval (..), Denial (..), Result (..))


data ApplicationStore a where
    Save      :: Application -> ApplicationStore ()
    Find      :: Guid Account -> ApplicationStore (Maybe Application)
    All       :: ApplicationStore [Application]

    SaveResult :: Guid Account -> Result -> ApplicationStore ()
    FindResult :: Guid Account -> ApplicationStore (Maybe Result)

instance (Selda m) => Service m ApplicationStore where
    run (Save a)         = save a
    run (Find i)         = loadById i
    run All              = loadAll
    run (SaveResult i r) = saveResult i r
    run (FindResult i)   = findResult i



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
