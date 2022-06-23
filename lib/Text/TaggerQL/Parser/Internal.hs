{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Parser.Internal (
  requestParser,
  sentenceParser,
  manyTermParser,
  termParser,
  subQueryParser,
  termPartialParser,
) where

import Control.Monad (void)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Tagger (QueryCriteria (..), SetOp (..))
import qualified Data.Text as T
import Text.Parsec (
  Parsec,
  ParsecT,
  Stream,
  choice,
  many1,
  noneOf,
  oneOf,
  optionMaybe,
  satisfy,
  sepBy1,
  spaces,
  try,
  (<?>),
  (<|>),
 )
import Text.TaggerQL.AST (
  Request (..),
  Sentence (..),
  Term (Term, termPredicate),
 )

type Parser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

type TermParser = Parser (Term T.Text)

type SentenceParser = Parser (Sentence T.Text)

type RequestParser = Parser (Request T.Text)

newtype ParserState = S {isTopLevel :: Bool} deriving (Show, Eq)

{- |
 Parses many 'Sentence`s to form a whole 'Request`.
-}
requestParser :: RequestParser
requestParser = Request <$> many1 sentenceParser

{- |
 Parses a 'Sentence` from many 'Term`s.

 Optionally wrapped in ().
-}
sentenceParser :: SentenceParser
sentenceParser = do
  maybeParen <- optionMaybe queryOpenParser
  terms <- manyTermParser
  maybe (pure ()) (const queryCloseParser) maybeParen
  return $ Sentence terms

{- |
 Parse one or more 'Term`s separated by 0 or more spaces.
-}
manyTermParser :: Parser [Term T.Text]
manyTermParser = sepBy1 termParser spaces

{- |
 Parse a 'Term` from a partial 'Term` and an optional subquery
 to determine the predicates.
-}
termParser :: TermParser
termParser = do
  partialTerm <- termPartialParser
  spaces
  predicates <- optionMaybe subQueryParser
  spaces
  return $ partialTerm{termPredicate = fromMaybe [] predicates}

{- |
 Parses 'Term`s wrapped in {} to form predicates for this parser's caller.
-}
subQueryParser :: Parser [Term T.Text]
subQueryParser = do
  subClauseScopeOpenParser
  terms <- sepBy1 termParser spaces
  subClauseScopeCloseParser
  return terms

{- |
 Parses a partial 'Term`, a term with no predicates by default.
-}
termPartialParser :: TermParser
termPartialParser = do
  so <- anyOpParser
  spaces
  qc <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  return $ Term so qc p []

anyOpParser :: SetOpParser
anyOpParser =
  (choice . map try $ [unionOpParser, intersectOpParser, diffOpParser]) <|> return Intersect

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
    <|> pure DescriptorCriteria

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
