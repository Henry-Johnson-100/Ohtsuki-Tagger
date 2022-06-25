{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Parser.Internal (
  combinableSentenceParser,
  sentenceParser,
  combinableTermParser,
  termTreeParser,
  termTreeChildrenParser,
  noRelationTreeParser,
  termParser,
) where

import Control.Monad (void, when)
import Data.Char (toLower)

import Data.Maybe
import Data.Tagger (QueryCriteria (..), SetOp (..))
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.AST

type Parser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

-- {- |
--  Parses many 'Sentence`s to form a whole 'Request`.
-- -}
-- requestParser :: RequestParser
-- requestParser = Request <$> many1 sentenceParser

combinableSentenceParser :: Parser (CombinableSentence T.Text)
combinableSentenceParser = do
  maybeSOWithParen <-
    optionMaybe
      ( do
          so <- explicitOpParser <|> pure Intersect
          spaces
          queryOpenParser
          return so
      )
  s <- sentenceParser
  so <- maybe (pure Intersect) (\so' -> queryCloseParser >> return so') maybeSOWithParen
  return $ CombinableSentence so s

{- |
 Parse a 'Sentence` of space-delimited 'CombinableTerm`s, optionally wrapped in
  parentheses.
-}
sentenceParser :: Parser (Sentence T.Text)
sentenceParser = do
  cts <- sepBy1 combinableTermParser spaces
  return $ Sentence cts

{- |
 Parse a 'CombinableTerm` which consists of an optional 'SetOp` literal
 and a single 'TermTree`. The 'SetOp` has a default value of 'Intersect` if none
  are parsed.
-}
combinableTermParser :: Parser (CombinableTerm T.Text)
combinableTermParser = do
  so <- explicitOpParser <|> pure Intersect
  spaces
  term <- termTreeParser True
  return $ CombinableTerm so term

{- |
 Parse a complete 'TermTree` including any nested children.
-}
termTreeParser ::
  -- | 'True` if the 'TermTree` is top-level.
  --This determines if simple 'Terms` are 'Simple` or 'Bottom`
  Bool ->
  Parser (TermTree T.Text)
termTreeParser isTopLevel = do
  basis <- noRelationTreeParser isTopLevel
  spaces
  predicates <- optionMaybe termTreeChildrenParser
  when
    (isJust predicates && (termCriteria . termTreeNode $ basis) == UntaggedCriteria)
    (fail "Cannot use UntaggedCriteria literal \"U.\" with a subquery.")
  spaces
  return $ maybe basis (newPredicates basis) predicates

{- |
 Parse all 'TermTree`s inside of a given subclause surrounded by {}
-}
termTreeChildrenParser :: Parser [TermTree T.Text]
termTreeChildrenParser = do
  subClauseScopeOpenParser
  t <- sepBy1 (termTreeParser False) spaces
  subClauseScopeCloseParser
  return t

{- |
 Parse a 'TermTree` without checking for children.
-}
noRelationTreeParser :: Bool -> Parser (TermTree T.Text)
noRelationTreeParser isTopLevel =
  if isTopLevel
    then Simple <$> termParser
    else Bottom <$> termParser

-- Simple <$> termParser

{- |
 Parse a 'Term` from a 'QueryCriteria` literal and text pattern.
-}
termParser :: Parser (Term T.Text)
termParser = do
  qc <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  return $ Term qc p

{- |
 Parses a 'SetOp` literal or nothing.
-}
explicitOpParser :: SetOpParser
explicitOpParser = choice . map try $ [unionOpParser, intersectOpParser, diffOpParser]

unionOpParser :: SetOpParser
unionOpParser = ichar 'u' >> ichar '|' >> return Union

intersectOpParser :: SetOpParser
intersectOpParser = ichar 'i' >> ichar '|' >> return Intersect

diffOpParser :: SetOpParser
diffOpParser = ichar 'd' >> ichar '|' >> return Difference

queryOpenParser :: Parser ()
queryOpenParser = void (ichar '(') >> spaces

queryCloseParser :: Parser ()
queryCloseParser = void spaces >> void (ichar ')')

subClauseScopeOpenParser :: Parser ()
subClauseScopeOpenParser = void (ichar '{') >> spaces

subClauseScopeCloseParser :: Parser ()
subClauseScopeCloseParser = spaces >> void (ichar '}')

anyCriteriaLiteralParser :: QueryCriteriaLiteralParser
anyCriteriaLiteralParser =
  ( choice . map try $
      [ descriptorCriteriaLiteralParser
      , metaDescriptorCriteriaLiteralParser
      , filePatternCriteriaLiteralParser
      , untaggedCriteriaLiteralParser
      ]
  )
    <|> pure MetaDescriptorCriteria

descriptorCriteriaLiteralParser :: QueryCriteriaLiteralParser
descriptorCriteriaLiteralParser = ichar 'd' >> ichar '.' >> return DescriptorCriteria

metaDescriptorCriteriaLiteralParser :: QueryCriteriaLiteralParser
metaDescriptorCriteriaLiteralParser =
  ichar 'r' >> ichar '.' >> return MetaDescriptorCriteria

filePatternCriteriaLiteralParser :: QueryCriteriaLiteralParser
filePatternCriteriaLiteralParser = ichar 'p' >> ichar '.' >> return FilePatternCriteria

untaggedCriteriaLiteralParser :: QueryCriteriaLiteralParser
untaggedCriteriaLiteralParser = ichar 'u' >> ichar '.' >> return UntaggedCriteria

{- |
 Case-insensitive 'Char` parser.
-}
ichar :: Stream s m Char => Char -> ParsecT s u m Char
ichar c = satisfy (\c' -> toLower c == toLower c')

acceptablePatternParser :: Parser T.Text
acceptablePatternParser =
  (fmap T.pack . many1 $ acceptableCharParser)
    <?> ( "an acceptable pattern of characters not in the set, '"
            ++ disallowedChars
            ++ "' or any of those characters with an escape char, \\, before it."
        )

acceptableCharParser :: Parser Char
acceptableCharParser =
  ( try
      ( do
          void $ ichar '\\'
          oneOf disallowedChars
      )
      <|> ichar '\\'
  )
    <|> notDisallowedChars

notDisallowedChars :: Parser Char
notDisallowedChars = noneOf disallowedChars

disallowedChars :: [Char]
disallowedChars = "{}()| \r\t\n"
