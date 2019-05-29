{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Text.XML.Parse
  ( Path(..)
  , Parser
  , ParseError(..)
  , ParserContext(..)
  , showPath
  , runParser
  , runParserWithContext
  , runParserDocument
  , XML.fromDocument
  , XML.fromNode
  , parseError
  -- * Selectors
  , element
  , elements
  , child
  , children
  , attribute
  , node
  , find
  -- * Content
  , content
  , text
  , optional
  , int
  , date
  , float
  , bool
  -- * Exports
  , XML.parseLBS
  , XML.parseText
  , XML.def
  , Node
  ) where


import           Control.Monad.Catch  (Exception)
import           Control.Monad.Except (Except, MonadError (catchError, throwError), runExcept)
import           Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import           Control.Monad.Reader (MonadReader (..))
import           Data.Function        ((&))
import qualified Data.List            as List
import           Data.Monoid          ((<>))
import           Data.Text            (Text, pack, unpack)
import           Data.Text.Read       (decimal, double, signed)
import           Data.Time.Calendar   (Day)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import           Data.Typeable        (Typeable)
import           Text.XML             (Document (..), Name (..), Node)
import qualified Text.XML             as XML
import           Text.XML.Cursor      (Cursor, ($/), ($//), (&.//))
import qualified Text.XML.Cursor      as XML


data Path
    = Child Text
    | Descendant Text
    | Attribute Text
    | Find
    | List Path
    deriving (Eq)

instance Show Path where
    show (Descendant n) = unpack $ "." <> n
    show (Attribute n)  = unpack $ "@" <> n
    show Find           = "?"
    show (List p)       = show p ++ "[]"
    show (Child n)      = unpack n


data ParseError = ParseError
    { errLocation :: [Path]
    , errAncestor :: [String]
    , errMessage  :: String
    , errContent  :: Text
    } deriving (Eq, Typeable)

instance Show ParseError where
    show (ParseError l as m c) =
      mconcat $ List.intersperse " "
        [ "ParseError"
        , mconcat $ List.intersperse "." $ reverse as
        , show l
        , m
        , show c
        ]

showPath :: [Path] -> String
showPath [] = "[]"
showPath ps = "." ++ (mconcat $ List.intersperse "." $ map show ps)

instance Exception ParseError


data ParserContext = ParserContext
    { location :: [Path]
    , cursor   :: Cursor
    }

newtype Parser a = Parser
    { parser :: (ReaderT ParserContext (Except ParseError) a)
    }

instance Functor Parser where
    fmap f (Parser m) = Parser (fmap f m)

instance Applicative Parser where
    pure a = Parser (pure a)
    Parser m <*> Parser n = Parser (m <*> n)

instance Monad Parser where
    (Parser ma) >>= f = Parser $ do
      a <- ma
      parser $ f a


instance MonadReader ParserContext Parser where
    ask = Parser ask
    local f (Parser ma) = Parser $ local f ma


instance MonadError ParseError Parser where
    throwError e = Parser $ throwError e
    catchError (Parser ma) f = Parser $ do
      catchError ma $ \e -> do
        parser $ f e



runParser :: Parser a -> Cursor -> Either ParseError a
runParser p c =
    runParserWithContext p $ emptyContext c


runParserDocument :: Parser a -> Document -> Either ParseError a
runParserDocument p d =
    runParserWithContext p $ emptyContext (XML.fromDocument d)


runParserWithContext :: Parser a -> ParserContext -> Either ParseError a
runParserWithContext (Parser p) ctx =
    runExcept $ runReaderT p ctx


setCursor :: Cursor -> ParserContext -> ParserContext
setCursor c ctx = ctx { cursor = c }


moveCursor :: [Cursor] -> Parser a -> Parser a
moveCursor [c] p =
  local (setCursor c) p
moveCursor [] _ =
  parseError "Element missing" ""
moveCursor cs _ =
  parseError "Element found too many" (pack $ show $ length cs)




addPath :: Path -> ParserContext -> ParserContext
addPath p = addPaths [p]

addPaths :: [Path] -> ParserContext -> ParserContext
addPaths ps ctx =
    ctx { location = location ctx <> ps }





single :: (Cursor -> [Cursor]) -> Parser a -> Parser a
single axis p = do
    c <- asks cursor
    p & moveCursor (axis c)


list :: (Cursor -> [Cursor]) -> Parser a -> Parser [a]
list axis p = do

    -- if we find nothing, throw an error
    c <- asks cursor
    case axis c of
      [] -> parseError "Element - missing" ""
      cs -> mapM each cs

  where
    each c' =  moveCursor [c'] p


element :: Name -> Parser a -> Parser a
element n p =
    single (findDescendants n) p
      & local (addPath $ Descendant $ XML.nameLocalName n)


-- gets multiple descendants
elements :: Name -> Parser a -> Parser [a]
elements n p = do
    list (findDescendants n) p
      & local (addPath $ List (Descendant $ XML.nameLocalName n))



child :: Name -> Parser a -> Parser a
child n p = do
    single (findChildren n) p
      & local (addPath $ Child $ XML.nameLocalName n)

children :: Name -> Parser a -> Parser [a]
children n p = do
    list (findChildren n) p
      & local (addPath $ List $ Child $ XML.nameLocalName n)



findDescendants :: Name -> Cursor -> [Cursor]
findDescendants (Name t _ _) c = c $// (XML.laxElement t)


findChildren :: Name -> Cursor -> [Cursor]
findChildren (Name t _ _) c = c $/ (XML.laxElement t)



attribute :: Name -> (Text -> Parser a) -> Parser a
attribute n parse =
  let name = XML.nameLocalName n
  in do c <- asks cursor
        case XML.laxAttribute name c of
          [t] -> parse t
          []  -> parseError "Attribute - missing" ""
          ts  -> parseError "Attribute - expected only one" (pack $ show ts)


content :: (Text -> Parser a) -> Parser a
content parse = do
    c <- asks cursor
    -- look for exactly one child content node
    case c $/ XML.content of
      [t] -> parse t
      []  -> parse ""
      ts  -> parseError "Content - expected only one" (pack $ show ts)


node :: Parser Node
node = do
  c <- asks cursor
  pure $ XML.node c


-- this is my cursor
-- innerCursor :: Parser Cursor
-- innerCursor = asks cursor



-- head :: Parser [a] -> Parser [String]
-- head parseElements = do

  -- here are ALL the cursors. Ooh does this work? I can work with this
  -- I want to get a list of cursors from a parser list

  -- Parser [a]: looks for the given node, makes sure it's a list, and parses it all
  -- I want to get a list of the cursors instead
  -- fmap?
  -- sort of
  -- I want 

  -- es <- parseElements
  -- cs <- mapM (\_ -> innerCursor) es :: Parser [Cursor]
  -- pure $ map dumpCursor cs
  -- can I do this with single / moveCursor?
  -- single requires an axis
  -- I no longer have an axis, because I've applied it already
  -- I only have a cursor
  -- parseElements has it's own cursor. What does that mean?
  -- can I pull it out?
  -- head takes a list of parsers and only runs the first one


-- Ok, what if you pass it in?

find :: Name -> Parser Bool -> Parser a -> Parser a
find n pb pa = do
  l <- asks location
  let axis = findDescendants n &.// XML.check (toCheck pb l)
  single axis pa
    & local (addPaths [List (Descendant $ XML.nameLocalName n), Find])



toCheck :: Parser Bool -> [Path] -> Cursor -> Bool
toCheck p ls c =
    -- this cursor passed in here is the one from check
    let ctx = ParserContext ls c
    in case runParserWithContext p ctx of
      Left _  -> False
      Right b -> b





parseError :: String -> Text -> Parser a
parseError e t = do
    l <- asks location
    c <- asks cursor
    throwError (ParseError l (ancestors c) e t)


ancestors :: Cursor -> [String]
ancestors c = map dumpCursor (c : XML.ancestor c)


dumpCursor :: Cursor -> String
dumpCursor c =
  case XML.node c of
    XML.NodeElement el ->
      unpack $ XML.nameLocalName $ XML.elementName el
    _ -> ""



date :: String -> Text -> Parser Day
date fmt t =
    case parseTimeM False defaultTimeLocale fmt (unpack t) of
      Nothing -> parseError "Cannot be parsed to date" t
      Just d  -> pure d



int :: Integral a => Text -> Parser a
int t = do
  (readError t) . (signed decimal) $ t


text :: Text -> Parser Text
text = pure


float :: Text -> Parser Float
float t = fmap realToFrac . (readError t) . (signed double) $ t


bool :: Text -> Parser Bool
bool "True"  = return True
bool "true"  = return True
bool "False" = return False
bool "false" = return False
bool "0"     = return False
bool "1"     = return True
bool "Y"     = return True
bool "y"     = return True
bool "N"     = return False
bool "n"     = return False
bool t       = parseError "Cannot be parsed to boolean" t



-- text :: Text -> Parser Text
-- text = pure


optional :: (Text -> Parser a) -> Text -> Parser (Maybe a)
optional _ ""   = return Nothing
optional _ "-1" = return Nothing
optional p t    = Just <$> p t



readError :: Text -> Either String (a, Text) -> Parser a
readError t (Left err)     = parseError err t
readError _ (Right (i, _)) = return i


emptyContext :: Cursor -> ParserContext
emptyContext c = ParserContext
  { location = []
  , cursor = c
  }
