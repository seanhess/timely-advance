{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
module SeldaTutorial where

-- import Database.Selda
-- import Database.Selda.PostgreSQL


data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
-- instance SqlType Pet

-- data Person = Person
--   { name :: Text
--   , age :: Int
--   , pet :: Maybe Pet
--   } deriving Generic
-- instance SqlRow Person

fatzo = "selda"
