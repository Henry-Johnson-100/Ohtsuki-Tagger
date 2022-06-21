{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Text.TaggerQL.Parser.Internal (
  ) where

import Control.Monad
import Data.Char
import Data.Tagger
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.AST

type SimpleParser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = SimpleParser QueryCriteria

type SetOpParser = SimpleParser SetOp

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