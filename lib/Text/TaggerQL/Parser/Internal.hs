{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Text.TaggerQL.Parser.Internal (
  termParser,
  Text.TaggerQL.Parser.Internal.parse,
) where

import Control.Monad
import Data.Char
import Data.Tagger
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.AST

parse :: SimpleParser a -> T.Text -> Either ParseError a
parse p = Text.Parsec.parse p "TaggerQL"

type SimpleParser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = SimpleParser QueryCriteria

type SetOpParser = SimpleParser SetOp

type TermParser = SimpleParser (Term T.Text)

type SentenceParser a = SimpleParser (Sentence a)

-- | placeholder
unionSentence :: SentenceParser [Term T.Text]
unionSentence = do
  r <-
    between
      (unionOpParser >> spaces)
      (ichar '|')
      (endBy termParser spaces)
  return $ Sentence Union r

termParser :: TermParser
termParser = try termWildCardParser <|> termPatternParser

termPatternParser :: TermParser
termPatternParser = do
  c <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  return $ TermPattern c p

termWildCardParser :: TermParser
termWildCardParser = do
  c <-
    anyCriteriaLiteralParser
  void $ ichar '*'
  return $ TermWildCard c

anyOpParser :: SetOpParser
anyOpParser =
  unionOpParser
    <|> intersectOpParser
    <|> diffOpParser

unionOpParser :: SetOpParser
unionOpParser = ichar 'u' >> ichar '|' >> return Union

intersectOpParser :: SetOpParser
intersectOpParser = ichar 'i' >> ichar '|' >> return Intersect

diffOpParser :: SetOpParser
diffOpParser = ichar 'd' >> ichar '|' >> return Difference

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

acceptablePatternParser :: SimpleParser T.Text
acceptablePatternParser = fmap T.pack . many1 $ notDisallowedChars

notDisallowedChars :: SimpleParser Char
notDisallowedChars = noneOf "{}| \r\t\n"

spaceOrEOF :: SimpleParser ()
spaceOrEOF = void space <|> eof