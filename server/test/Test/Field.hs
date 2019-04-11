{-# LANGUAGE OverloadedStrings #-}
module Test.Field where



import Data.Text (Text)
import qualified Data.Aeson as Aeson
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import Database.Selda.Field (Field(..))
import Database.Selda.SqlType (Lit(..), SqlType(..))



tests :: Tests ()
tests = do
    group "json" $ do
      test "json encoding the same" $ do
        let val = "one" :: Text
        Aeson.encode val @?= Aeson.encode (Field val)


    group "sql" $ do
      let val = "hello" :: Text
      let enc = "\"hello\"" :: Text

      test "lit text of json data" $ do
        mkLit enc @?= LText enc

      test "lit field" $ do
        mkLit (Field val) @?= LCustom (LText enc)



