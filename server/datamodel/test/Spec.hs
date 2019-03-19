{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson       as Aeson
import           Data.Model.Guid  as Guid
import           Data.Typeable    (Typeable)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.HttpApiData  as Http


data Stuff
instance GuidPrefix Stuff

guid :: Guid Stuff
guid = "asdf"

data SomethingElse
instance GuidPrefix SomethingElse where
  guidPrefix _ = "se"


main :: IO ()
main = defaultMain $
  testGroup "guid"
    [ testCase "parse" $ do
        guid @?= fromText "asdf"
        guid @?= fromText "stuff-asdf"
    , testCase "show" $ do
        show guid @?= "stuff-asdf"
    , testCase "toText" $ do
        toText guid @?= "stuff-asdf"
    , testCase "toJSON" $ do
        Aeson.toJSON guid @?= String "stuff-asdf"
    , testCase "parseJSON" $ do
        Aeson.eitherDecode "\"stuff-asdf\"" @?= Right guid
        Aeson.eitherDecode "\"asdf\"" @?= Right guid
    , testCase "toHttpApiData" $ do
        Http.toUrlPiece guid @?= "stuff-asdf"
    , testCase "fromHttpApiData" $ do
        Http.parseUrlPiece "stuff-asdf" @?= Right guid
        Http.parseUrlPiece "asdf" @?= Right guid
    , testCase "multiple hyphens" $ do
        fromText "stuff-one-two-three" @?= ("three" :: Guid Stuff)
        fromText "one-two-three" @?= ("three" :: Guid Stuff)
    , testCase "custom prefix" $ do
        let g = fromText "1234" :: Guid SomethingElse
        g @?= fromText "se-1234"
        toText g @?= "se-1234"
    ]
