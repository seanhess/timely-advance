{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
module BeamTutorial where

import Database.Beam
import Database.Beam.Postgres (connect, defaultConnectInfo, runBeamPostgresDebug, PgSelectSyntax)

import Data.Text (Text)

data UserT f = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text
    } deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
instance Beamable UserT
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

-- userKey = UserId "john@doe.org"

data ShoppingCartDb f = ShoppingCartDb
    { _shoppingCartUsers :: f (TableEntity UserT)
    } deriving Generic

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings


test :: IO ()
test = do
    conn <- connect defaultConnectInfo

    putStrLn "Sorted Users"
    queryUsersSort conn

    putStrLn "Bounded Users"
    queryBounded conn

    putStrLn "Count"
    queryCount conn

    putStrLn "Count Names"
    queryCountNames conn



  where
    addUsers conn = do
      runBeamPostgresDebug putStrLn conn $ runInsert $
        insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

    queryUsers conn = do
      let allUsers = all_ (_shoppingCartUsers shoppingCartDb)

      runBeamPostgresDebug putStrLn conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . print) users


    queryUsersSort conn = do
      let sortUsersByFirstName = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) (all_ (_shoppingCartUsers shoppingCartDb))

      runBeamPostgresDebug putStrLn conn $ do
        users <- runSelectReturningList $ select sortUsersByFirstName
        mapM_ (liftIO . putStrLn . show) users


    queryBounded conn = do
      let boundedQuery :: Q PgSelectSyntax _ _ _
          boundedQuery = limit_ 1 $ offset_ 1 $
                        orderBy_ (asc_ . _userFirstName) $
                        all_ (_shoppingCartUsers shoppingCartDb)

      runBeamPostgresDebug putStrLn conn $ do
        users <- runSelectReturningList (select boundedQuery :: SqlSelect PgSelectSyntax _)
        mapM_ (liftIO . putStrLn . show) users


    queryCount conn = do
      let userCount = aggregate_ (\u -> countAll_) (all_ (_shoppingCartUsers shoppingCartDb))

      runBeamPostgresDebug putStrLn conn $ do
        Just c <- runSelectReturningOne $ select userCount
        liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")

    queryCountNames conn = do
      let numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), countAll_)) $
                                all_ (_shoppingCartUsers shoppingCartDb)

      runBeamPostgresDebug putStrLn conn $ do
        countedByName <- runSelectReturningList $ select numberOfUsersByName
        mapM_ (liftIO . putStrLn . show) countedByName
