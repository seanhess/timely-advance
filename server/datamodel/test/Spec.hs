{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson       as Aeson
import Data.Model.Guid  as Guid
import Data.Model.Meta  as Meta
import Data.Model.Money as Money
import Data.Text        (Text)
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
import Test.Tasty
import Test.Tasty.HUnit
import Web.HttpApiData  as Http


data Stuff
instance GuidPrefix Stuff

guid :: Guid Stuff
guid = "asdf"

data SomethingElse
instance GuidPrefix SomethingElse where
  guidPrefix _ = "se"



data Info = Info
  { info :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Info
instance FromJSON Info

data User = User
  { name :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User



main :: IO ()
main = defaultMain $
  testGroup "tests"
   [ testGroup "money"
      [ testCase "cents equals dollars" $ do
          Money.fromFloat 100 @=? Money.fromCents 10000
      , testCase "cents both directions" $ do
          Money.toCents (Money.fromCents 100) @=? 100
      , testCase "dollars both directions" $ do
          Money.toFloat (Money.fromFloat 100) @=? 100
      , testCase "negative dollars" $ do
          Money.toFloat (Money.fromFloat (-100)) @=? (-100)
      ]

   , testGroup "guid"
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


    , testGroup "meta"
        [ testCase "serialize" $ do
            let meta = Meta (Info "info") (User "user") :: Meta "" Info User
            (encode meta) @?= "{\"name\":\"user\",\"info\":\"info\"}"

        , testCase "decode" $ do
            let meta = Meta (Info "info") (User "user") :: Meta "" Info User
            (eitherDecode $ encode meta) @?= (Right meta)

        , testCase "non-object" $ do
            let meta = Meta "hi" (User "user") :: Meta "id" Text User
            (encode meta) @?= "{\"name\":\"user\",\"id\":\"hi\"}"

        , testCase "non-object decode" $ do
            let meta = Meta "hi" (User "user") :: Meta "id" Text User
            (eitherDecode $ encode meta) @?= (Right $ meta)

        ]
    ]
