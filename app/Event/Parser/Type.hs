{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Redundant return" #-}

-- for this will just be a testing ground for traversing parsers, not just the
-- type declarations

module Event.Parser.Type () where

import Control.Monad.Trans.Class
import qualified Data.IntSet as IS
import Data.Maybe
import qualified Data.Text as T
import Database.Tagger.Access
import Event.Parser (PseudoDescriptor (..))
import Text.Parsec
import Type.Model.Prim

type KeySet a = IS.IntSet

type QueryParserT m a = ParsecT T.Text AppQueryInformation m a

runQueryParser :: QueryParserT IO (KeySet FileKey) -> T.Text -> IO (KeySet FileKey)
runQueryParser p t = do
  r <- runParserT p (AppQueryInformation Union ByTag) "Query" t
  either (\e -> print e >> return []) return r

newtype SubjectPhrase a = SubjectPhrase {phraseContents :: a}
  deriving (Show, Eq, Functor, Foldable)

data AppQueryInformation = AppQueryInformation
  { aqiSetArithmetic :: !FileSetArithmetic,
    aqiQueryCriteria :: !QueryCriteria
  }
  deriving (Show, Eq)

unionLiteralParser :: QueryParserT IO ()
unionLiteralParser = do
  oneOf "Uu"
  char '|'
  modifyState (\s -> s {aqiSetArithmetic = Union})

intersectLiteralParser :: QueryParserT IO ()
intersectLiteralParser = do
  oneOf "Ii"
  char '|'
  modifyState (\s -> s {aqiSetArithmetic = Intersect})

diffLiteralParser :: QueryParserT IO ()
diffLiteralParser = do
  oneOf "Dd"
  char '|'
  modifyState (\s -> s {aqiSetArithmetic = Diff})

byTagLiteralParser :: QueryParserT IO ()
byTagLiteralParser = do
  oneOf "Tt"
  char '.'
  modifyState (\s -> s {aqiQueryCriteria = ByTag})

byRelationLiteralParser :: QueryParserT IO ()
byRelationLiteralParser = do
  oneOf "Rr"
  char '.'
  modifyState (\s -> s {aqiQueryCriteria = ByRelation})

byPatternLiteralParser :: QueryParserT IO ()
byPatternLiteralParser = do
  oneOf "Pp"
  char '.'
  modifyState (\s -> s {aqiQueryCriteria = ByPattern})

noQueryLiteralParser :: QueryCriteria -> QueryParserT IO ()
noQueryLiteralParser qc = modifyState (\s -> s {aqiQueryCriteria = qc})

queryTokenResultParser ::
  Connection ->
  QueryCriteria ->
  QueryParserT IO (KeySet FileKey)
queryTokenResultParser c qc = do
  p <- queryTokenParser qc
  -- have a case function here for different query criteria
  lift $ queryByTagPattern c p

queryByTagPattern :: Connection -> T.Text -> IO (KeySet FileKey)
queryByTagPattern c p = do
  fks <- lookupFilesHavingDescriptorPattern c (PDescriptor p)
  return . IS.fromList $ fks

queryTokenParser :: QueryCriteria -> QueryParserT IO T.Text
queryTokenParser qc = do
  try
    ( byTagLiteralParser
        <|> byRelationLiteralParser
        <|> byPatternLiteralParser
    )
    <|> noQueryLiteralParser qc
  primTokenParser

-- | Parses a valid token
primTokenParser :: QueryParserT IO T.Text
primTokenParser =
  fmap T.pack . many1 . noneOf $ "{} \t\n"