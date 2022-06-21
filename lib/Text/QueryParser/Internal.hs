{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Text.QueryParser.Internal (
  Term (..),
  Phrase (..),
  phraseParser,
  termParser,
) where

import Control.Monad
import Data.Tagger
import qualified Data.Text as T
import Text.Parsec

data Term a
  = TermPattern QueryCriteria a
  | TermWildCard QueryCriteria
  deriving (Show, Eq)

data Phrase a
  = PhraseSimple (Term a)
  | PhraseCompound (Term a) [Phrase a]
  deriving (Show, Eq)

type SimpleParser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = SimpleParser QueryCriteria

type TermParser = SimpleParser (Term T.Text)

type PhraseParser = SimpleParser (Phrase T.Text)

phraseParser :: PhraseParser
phraseParser = undefined

phraseCompoundParser :: PhraseParser
phraseCompoundParser = undefined

phraseBasisParser :: TermParser
phraseBasisParser = do
  t <- termParser
  spaces
  return t

termParser :: TermParser
termParser = try termWildCardParser <|> termPatternParser

termPatternParser :: TermParser
termPatternParser = do
  c <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  spaceOrEOF
  return $ TermPattern c p

termWildCardParser :: TermParser
termWildCardParser = do
  c <-
    anyCriteriaLiteralParser
  void $ char '*'
  void spaceOrEOF
  return $ TermWildCard c

anyCriteriaLiteralParser :: QueryCriteriaLiteralParser
anyCriteriaLiteralParser =
  descriptorCriteriaLiteralParser
    <|> metaDescriptorCriteriaLiteralParser
    <|> filePatternCriteriaLiteralParser
    <|> untaggedCriteriaLiteralParser
    <|> pure DescriptorCriteria

descriptorCriteriaLiteralParser :: QueryCriteriaLiteralParser
descriptorCriteriaLiteralParser = do
  void $ char 'd'
  void $ char '.'
  return DescriptorCriteria

metaDescriptorCriteriaLiteralParser :: QueryCriteriaLiteralParser
metaDescriptorCriteriaLiteralParser = do
  void $ char 'r'
  void $ char '.'
  return MetaDescriptorCriteria

filePatternCriteriaLiteralParser :: QueryCriteriaLiteralParser
filePatternCriteriaLiteralParser = do
  void $ char 'p'
  void $ char '.'
  return FilePatternCriteria

untaggedCriteriaLiteralParser :: QueryCriteriaLiteralParser
untaggedCriteriaLiteralParser = do
  void $ char 'u'
  void $ char '.'
  return UntaggedCriteria

acceptablePatternParser :: SimpleParser T.Text
acceptablePatternParser = fmap T.pack . many1 $ notDisallowedChars

notDisallowedChars :: SimpleParser Char
notDisallowedChars = noneOf "{} \r\t\n"

spaceOrEOF :: SimpleParser ()
spaceOrEOF = void space <|> eof