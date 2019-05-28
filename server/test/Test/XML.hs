{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Test.XML where


-- import Data.String.Conversions (cs)
import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Text.XML         (def, parseLBS_)
import           Text.XML.Parse


specXML = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" ts

tests :: Tests ()
tests = do
    let doc = parseLBS_ def inputXml
    let cur = fromDocument doc

    group "parse" $ do

      test "should fail to find node" $ do
        let (!Left _) = flip runParser cur $ element "asdf" $ content text
        pure ()

      test "should parse single node" $ do
        let t = flip runParser cur $ element "ugly" $ content text
        t @?= Right "Ugly"

      test "should parse multiple nodes" $ do
        let ts = flip runParser cur $ elements "lava" $ content text
        ts @?= Right ["Lava", "Woot"]

      test "parse attribute text" $ do
        let ts = flip runParser cur $ element "mega" $ attribute "gender" text
        ts @?= Right "man"

      test "parse attribute fails" $ do
        let (!Left _) = flip runParser cur $ element "mega" $ attribute "gender" int
        pure ()

      test "parse attribute type" $ do
        let ts = flip runParser cur $ element "mega" $ attribute "gender" gender
        ts @?= Right Man

      test "parse <person> as single fails" $ do
        let (!Left _) = flip runParser cur $ element "person" $ person
        pure ()

      test "parse people" $ do
        let (!Right ps) = flip runParser cur $ elements "person" person
        length ps @?= 4


      test "target stuff lava" $ do
        (flip runParser cur $ element "stuff" $ element "lava" $ content text) @?= Right "Woot"

      test "targeting the root is a miss" $ do
        let (!Left _) = flip runParser cur $ element "root" $ element "lava" $ content text
        pure ()

      test "target child" $ do
        let t = flip runParser cur $ child "lava" $ content text
        t @?= Right "Lava"
        -- print l
        pure ()


    group "errors" $ do
      test "path" $ do
        let (!Left e) = flip runParser cur $ elements "person" $ child "kids" $ attribute "age" int
        errLocation e @?= [List (Descendant "person"), Child "kids"]

      test "serialize" $ do
        showPath [List (Descendant "person"), Child "kids"] @?= "..person[].kids"
        showPath [Child "kids", Attribute "age"] @?= ".kids.@age"


    group "children" $ do
      test "should error if more than one" $ do
        let (!Left _) = flip runParser cur $ elements "stuff" $ child "person" person
        pure ()

      test "should find children" $ do
        let (!Right cs) = flip runParser cur $ element "stuff" $ children "person" person :: Either ParseError [Person]
        length cs @?= 2




data Gender
  = Man
  | Woman
  deriving (Show, Eq)


gender :: Text -> Parser Gender
gender "man"   = pure Man
gender "woman" = return Woman
gender t       = parseError "expected gender" t



data Person = Person Int Text Text deriving Show


person :: Parser Person
person = do
  age  <- attribute "age" int :: Parser Int
  good <- attribute "goodAtHaskell" text
  name <- content text
  -- fat <- int
  pure $ Person age good name




inputXml = mconcat
  [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
  , "<root>"
  , "  <lava>Lava</lava>"
  , "  <people>"
  , "    <person age=\"18\" goodAtHaskell=\"yes\">Fatty McGee</person>"
  , "  </people>"
  , "  <stuff>"
  , "    <person age=\"25\" goodAtHaskell=\"yes\">Michael</person>"
  , "    <lava>Woot</lava>"
  , "    <mega gender=\"man\">WOOT</mega>"
  , "    <person age=\"2\" goodAtHaskell=\"bad\">Eliezer</person>"
  , "  </stuff>"
  , "  <ugly>Ugly</ugly>"
  , "  <person age=\"2\" goodAtHaskell=\"awesome\">Bobby</person>"
  , "</root>"
  ]

