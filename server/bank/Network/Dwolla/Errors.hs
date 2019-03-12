{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.Dwolla.Errors where


import           Control.Monad.Catch  (Exception)
import           Data.Aeson           (FromJSON)
import Control.Applicative ((<|>))
import qualified Data.Aeson           as Aeson
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Model.Id        (Id (..))
import           Data.Text            as Text
import           GHC.Generics         (Generic)
import           Network.Dwolla.Types (RelLink (..), resourceToId)
import           Servant.Client       (GenResponse (..), ServantError (..))


dwollaError :: ServantError -> DwollaError
dwollaError err@(FailureResponse (Response _ _ _ body)) =
  fromMaybe (ApiError err) $ do
    dup <- validationErrors body <|> duplicateResource body
    about <- Map.lookup "about" $ _links dup
    id    <- resourceToId $ href about
    pure $ Duplicate id

  where
    isDuplicate (ValidationError c _ _ _) = c == "Duplicate"

    validationErrors body = do
      info  <- Aeson.decode body
      List.find isDuplicate $ errors $ _embedded info

    duplicateResource body =
      Aeson.decode body

dwollaError err = ApiError err



data DwollaError
  = BadLocation
  | ApiError ServantError
  | Duplicate (Id ())
  deriving (Show, Eq, Generic)

instance Exception DwollaError

-- we don't KNOW that we can parse this
data ApiErrorContent = ApiErrorContent
  { code      :: Text
  , message   :: Text
  , _embedded :: EmbeddedErrors
  } deriving (Generic)

instance FromJSON ApiErrorContent

data EmbeddedErrors = EmbeddedErrors
  { errors :: [ValidationError]
  } deriving (Generic)

instance FromJSON EmbeddedErrors

data ValidationError = ValidationError
  { code    :: Text
  , message :: Text
  , path    :: Maybe Text
  , _links  :: Map Text (RelLink ())
  } deriving (Generic)

instance FromJSON ValidationError
