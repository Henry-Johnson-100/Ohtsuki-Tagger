{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use let" #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- for this will just be a testing ground for traversing parsers, not just the
-- type declarations

module Event.Parser.Type () where

import Control.Monad
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

parseQuery ::
  QueryParserT IO () ->
  AppQueryInformation ->
  T.Text ->
  IO AppQueryInformation
parseQuery p aqi =
  either (const (return aqi) <=< print) return
    <=< runParserT (runQueryParser p) aqi "Query"
  where
    runQueryParser ::
      QueryParserT IO () ->
      QueryParserT IO AppQueryInformation
    runQueryParser p = do
      p
      s <- getState
      return s

-- This will be moved to Type.Model.Prim eventually.
data AppQueryInformation = AppQueryInformation
  { aqiSetArithmetic :: !FileSetArithmetic,
    aqiQueryCriteria :: !QueryCriteria,
    aqiResultSet :: !(KeySet FileKey)
  }
  deriving (Show, Eq)

-- | Combine given keyset with the state
-- using the state's set arithmetic to determine how to perform the combination.
combineWithState :: KeySet FileKey -> AppQueryInformation -> AppQueryInformation
combineWithState fks aqi@(AppQueryInformation a _ cfks) =
  aqi {aqiResultSet = setArithmeticToIntSetOperation a fks cfks}

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

eofSetArithmeticParser :: FileSetArithmetic -> QueryParserT IO ()
eofSetArithmeticParser a = do
  eof
  modifyState (\s -> s {aqiSetArithmetic = a})

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

setArithmeticToIntSetOperation ::
  FileSetArithmetic ->
  IS.IntSet ->
  IS.IntSet ->
  IS.IntSet
setArithmeticToIntSetOperation a =
  case a of
    Union -> IS.union
    Intersect -> IS.intersection
    Diff -> IS.difference

queryParser :: Connection -> QueryParserT IO ()
queryParser = const (return ()) <=< many . topLevelPhraseStatementParser

topLevelPhraseStatementParser ::
  Connection ->
  QueryParserT IO ()
topLevelPhraseStatementParser c = do
  setArithmeticParser -- set state arithmetic for the phrase
  spaces
  words <-
    manyTillNoConsume
      ( do
          w <- queryWordParser c
          spaces
          return w
      )
      (setArithmeticParser <|> eof)
  spaces
  let wordSet = IS.unions words
  modifyState . combineWithState $ wordSet
  where
    setArithmeticParser =
      try . optional $
        unionLiteralParser
          <|> intersectLiteralParser
          <|> diffLiteralParser

-- | Like manyTill but does not consume any of end.
manyTillNoConsume ::
  Stream s m t =>
  ParsecT s u m a ->
  ParsecT s u m end ->
  ParsecT s u m [a]
manyTillNoConsume p end = manyTill p (lookAhead end)

-- | Parses either a queryToken or a subQuery
queryWordParser ::
  Connection ->
  QueryParserT IO (KeySet FileKey)
queryWordParser c =
  try
    ( do
        tr <- queryTokenParser
        lift $ placeholderParserIO c tr
    )
    <|> subQueryParser c

queryTokenParser :: QueryParserT IO T.Text
queryTokenParser = do
  try
    ( byTagLiteralParser
        <|> byRelationLiteralParser
        <|> byPatternLiteralParser
    )
  t <- primTokenParser
  spaces
  notFollowedBy (char '{')
  return t

subQueryParser ::
  Connection ->
  QueryParserT IO (KeySet FileKey)
subQueryParser c = undefined
  where
    subQueryPhraseParser = undefined

-- | Parses and queries the inside of a subquery's {}
subQueryContentsParser ::
  Connection ->
  KeySet FileKey ->
  QueryParserT IO (KeySet FileKey)
subQueryContentsParser c subjects = undefined

-- | Parses a valid token
primTokenParser :: QueryParserT IO T.Text
primTokenParser =
  fmap T.pack . many1 . noneOf $ "{} \t\n"

placeholderParserIO :: Connection -> T.Text -> IO (KeySet FileKey)
placeholderParserIO c p = do
  fks <- lookupFilesHavingDescriptorPattern c (PDescriptor p)
  return . IS.fromList $ fks