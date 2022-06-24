{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Parser.Internal (
  clauseParser,
  termTreeParser,
  termTreeChildrenParser,
  simpleTreeParser,
  termParser,
) where

import Control.Monad (guard, void, when)
import Data.Char (toLower)

import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Tagger (QueryCriteria (..), SetOp (..))
import qualified Data.Text as T
import GHC.Stack
import Text.Parsec
import Text.TaggerQL.AST

type Parser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

newtype ParserState = S {isTopLevel :: Bool} deriving (Show, Eq)

-- {- |
--  Parses many 'Sentence`s to form a whole 'Request`.
-- -}
-- requestParser :: RequestParser
-- requestParser = Request <$> many1 sentenceParser

-- {- |
--  Parses a 'Sentence` from many 'Term`s.

--  Optionally wrapped in ().
-- -}
-- sentenceParser :: SentenceParser
-- sentenceParser = do
--   maybeParen <- optionMaybe queryOpenParser
--   terms <- manyTermParser
--   maybe (pure ()) (const queryCloseParser) maybeParen
--   return $ Sentence terms

-- {- |
--  Parse one or more 'Term`s separated by 0 or more spaces.
-- -}
-- manyTermParser :: Parser [Term T.Text]
-- manyTermParser = sepBy1 termParser spaces

{- |
 Parse a 'Clause` which consists of an optional 'SetOp` literal
 and any number of 'TermTree`s delimited by spaces.

 Parses a clauses until another 'SetOp` parser, unescaped closing parenthesis '(', or eof.
-}
clauseParser :: Parser (Clause T.Text)
clauseParser = do
  so <- anyOpParser
  spaces
  terms <-
    manyTill1
      (do t <- termTreeParser; spaces; return t)
      ( try
          ( void unionOpParser
              <|> void intersectOpParser
              <|> void diffOpParser
              <|> void queryCloseParser
              <|> eof
          )
      )
      <?> "at least one term in a Clause"
  return $ Clause so (NE.fromList terms)

manyTill1 ::
  (Stream s m t, HasCallStack) =>
  ParsecT s u m a ->
  ParsecT s u m end ->
  ParsecT s u m [a]
manyTill1 p e = do
  r <- manyTill p e
  guard (not . null $ r) <?> "at least one result in manyTill1"
  return r

{- |
 Parse a complete 'TermTree` including any nested children.
-}
termTreeParser :: Parser (TermTree T.Text)
termTreeParser = do
  basis <- simpleTreeParser
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
  t <- sepBy1 termTreeParser spaces
  subClauseScopeCloseParser
  return t

{- |
 Parse a 'TermTree` without checking for children.
-}
simpleTreeParser :: Parser (TermTree T.Text)
simpleTreeParser = Simple <$> termParser

{- |
 Parse a 'Term` from a 'QueryCriteria` literal and text pattern.
-}
termParser :: Parser (Term T.Text)
termParser = do
  qc <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  return $ Term qc p

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
