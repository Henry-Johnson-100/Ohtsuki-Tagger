{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Parser.Internal (
  taggerQLTokenParser,
  taggerQLSubClauseParser,
  taggerQLComplexTermParser,
  taggerQLSimpleTermParser,
) where

import Control.Monad
import Data.Char
import Data.Tagger
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.AST

type Parser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

type TaggerQLTokenParser = Parser (TaggerQLToken T.Text)

type TaggerQLComplexTermParser = Parser (TaggerQLComplexTerm T.Text)

type TaggerQLSubClauseParser = Parser (TaggerQLSubClause T.Text)

type TaggerQLSimpleTermParser = Parser (TaggerQLSimpleTerm T.Text)

{- |
 Parses a 'TaggerQLToken`

 This is the terminating parser for the recursive definition of the
 'taggerQLComplexTermParser`
-}
taggerQLTokenParser :: TaggerQLTokenParser
taggerQLTokenParser =
  try taggerQLSimpleTokenParser
    <|> taggerQLComplexTokenParser
 where
  taggerQLSimpleTokenParser = TaggerQLSimpleToken <$> taggerQLSimpleTermParser
  taggerQLComplexTokenParser = TaggerQLComplexToken <$> taggerQLComplexTermParser

{- |
 Parses a 'TaggerQLComplexTerm`

 recursively defined with 'taggerQLSubClauseParser` and 'taggerQLTokenParser`
-}
taggerQLComplexTermParser :: TaggerQLComplexTermParser
taggerQLComplexTermParser = do
  c <-
    descriptorCriteriaLiteralParser
      <|> metaDescriptorCriteriaLiteralParser
      <|> filePatternCriteriaLiteralParser
      <|> try
        ( untaggedCriteriaLiteralParser
            >> unexpected "Untagged queries not allowed before a subquery."
        )
      <|> pure DescriptorCriteria
  basis <- acceptablePatternParser
  spaces
  against <- taggerQLSubClauseParser
  spaces
  return $
    TaggerQLComplexTerm
      c
      basis
      against

{- |
 Parses a 'TaggerQLSubClause`
-}
taggerQLSubClauseParser :: TaggerQLSubClauseParser
taggerQLSubClauseParser = do
  setOp <- anyOpParser
  contents <- subClauseContentsParser (sepEndBy taggerQLTokenParser (many space))
  spaces
  return $ TaggerQLSubClause setOp contents
 where
  subClauseContentsParser = between subClauseOpenParser subClauseCloseParser

{- |
 Parses a 'TaggerQLSimpleTerm`

 The smallest unit of complete TaggerQL syntax.
-}
taggerQLSimpleTermParser :: TaggerQLSimpleTermParser
taggerQLSimpleTermParser = do
  c <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  notFollowedBy (spaces >> anyOpParser >> subClauseOpenParser)
  return $ TaggerQLSimpleTerm c p

anyOpParser :: SetOpParser
anyOpParser =
  unionOpParser
    <|> intersectOpParser
    <|> diffOpParser

unionOpParser :: SetOpParser
unionOpParser = ichar 'u' >> return Union

intersectOpParser :: SetOpParser
intersectOpParser = ichar 'i' >> return Intersect

diffOpParser :: SetOpParser
diffOpParser = ichar 'd' >> return Difference

subClauseOpenParser :: Parser ()
subClauseOpenParser = void (ichar '[') >> spaces

subClauseCloseParser :: Parser ()
subClauseCloseParser = spaces >> void (ichar ']')

anyCriteriaLiteralParser :: QueryCriteriaLiteralParser
anyCriteriaLiteralParser =
  descriptorCriteriaLiteralParser
    <|> metaDescriptorCriteriaLiteralParser
    <|> filePatternCriteriaLiteralParser
    <|> untaggedCriteriaLiteralParser
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
acceptablePatternParser = fmap T.pack . many1 $ acceptableCharParser

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
disallowedChars = "{}()[] \r\t\n"

subqueryContents :: Parser a -> Parser a
subqueryContents = between (char '{') (char '}')

spaceOrEOF :: Parser ()
spaceOrEOF = void space <|> eof