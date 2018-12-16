{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, TypeOperators #-}
module SeldaTutorial where
import           Database.Selda
import           Database.Selda.SQLite
-- import Control.Monad.Exception (try)
-- import           Database.Selda.PostgreSQL

data Pet = Dog | Horse | Dragon
    deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { pid :: ID Person
  , name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving (Generic, Show)
instance SqlRow Person

data Home = Home
  { ownerName :: Text
  , city      :: Text
  , rent      :: Int
  } deriving (Generic, Show)
instance SqlRow Home

people :: Table Person
people = table "people" [#pid :- autoPrimary]

homes :: Table Home
homes = table "homes" []

selda :: IO ()
selda = withSQLite "people3.sqlite" $ do

  -- createTable homes
  -- createTable people
  -- n <- petsForOne
  -- liftIO $ print n

  -- _ <- ageEveryone

  -- ps <- insertThenInspect
  -- liftIO $ print ps
  -- insertSara





  -- ps <- query personsFromHomes
  -- mapM_ (liftIO . print) ps

  _ <- homesForEveryone


  -- insert_ people [Person def "Bob" 50 Nothing]
  -- insert_ homes [Home "Will" "SLC" 2000]

  liftIO $ putStrLn "PEOPLE WITH HOMES"
  mapM_ (liftIO . print) =<< query peopleWithHomes

  liftIO $ putStrLn "PEOPLE"
  mapM_ (liftIO . print) =<< query (select people)

  liftIO $ putStrLn "HOMES"
  mapM_ (liftIO . print) =<< query (select homes)


  -- liftIO $ print persons
  -- insertAndPrintPk
  -- insert_ people
  --   [Person def "Sara" 14 Nothing]


pets :: SeldaM ()
pets = do
  adultsAndTheirPets <- query $ do
    person <- select people
    let t = 18 :: Int
    restrict (person ! #age .>= int t)
    return (person ! #name :*: person ! #pet)
  liftIO $ print adultsAndTheirPets

insertSara :: SeldaM ()
insertSara = do
    insert_ people [Person def "Sara" 18 Nothing]
    insert_ homes [Home "Sara" "Austin" 1200]

insertThenInspect :: SeldaM [Person]
insertThenInspect = do
  insertSara
  query (select people)

insertAndPrintPk :: SeldaM ()
insertAndPrintPk = do
  pk <- insertWithPK people [Person def "Sara" 14 Nothing]
  liftIO (print pk)

findPerson :: SeldaM [Person]
findPerson = query $ limit 0 1 $ do
    person <- select people

    restrict (person ! #pid .== literal (toId 3))
    -- restrict (person ! #name .== "Sara")
    return person


petsForOne :: SeldaM Int
petsForOne =
    update people (\person -> person ! #pid .== literal (toId 3))
                  (\person -> person `with` [#pet := just (literal Dragon)])


ageEveryone :: SeldaM Int
ageEveryone =
  update people (const true) (\person -> person `with` [#age += 1])


peopleWithHomes :: Query s (Row s Person :*: Row s Home)
peopleWithHomes = do
  person <- select people
  home <- select homes

  -- this is a comprehension, without it it matches every person with every home
  restrict (person ! #name .== home ! #ownerName)
  return (person :*: home)

homelessPeople :: Query s (Row s Person)
homelessPeople = do
  person <- select people
  restrict (not_ $ (person ! #name) `isIn` (#ownerName `from` select homes))
  return person

personsFromHomes :: Query s (Row s Person)
personsFromHomes = do
  home <- select homes
  return $ new [#name := home ! #ownerName]

homesForEveryone :: SeldaM Int
homesForEveryone = queryInto homes $ do
  person <- select people
  restrict (not_ $ (person ! #name) `isIn` (#ownerName `from` select homes))

  return $ new
    [ #ownerName := person ! #name
    , #city := "Tokyo"
    , #rent := 50000
    ]

