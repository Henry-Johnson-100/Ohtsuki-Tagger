{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.NewAST.Parser (
  parseExpression,
) where

import Control.Monad (void, when)
import Data.Char (toLower)
import Data.Functor (($>))
import Data.Tagger (QueryCriteria (..), SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (
  ParseError,
  Parsec,
  ParsecT,
  Stream,
  chainl1,
  choice,
  many1,
  noneOf,
  notFollowedBy,
  oneOf,
  optionMaybe,
  parse,
  satisfy,
  spaces,
  try,
  (<?>),
  (<|>),
 )
import Text.TaggerQL.NewAST.AST (
  ComplexTerm (Bottom, (:<-)),
  Expression (..),
  Term (Term),
  TermArity (NAry, Nullary),
 )

type Parser a = Parsec Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

parseExpression :: Text -> Either ParseError Expression
parseExpression = parse expressionParser "TaggerQL"

expressionParser :: Parser Expression
expressionParser =
  chainl1
    ( spaces
        *> ( try parenthesizedExpression
              <|> ( (\qc p -> Value . Nullary $ Term qc p)
                      <$> anyCriteriaLiteralParser <*> acceptablePatternParser
                  )
           )
    )
    (spaces *> setOpOperator explicitOpParser)

parenthesizedExpression :: Parser Expression
parenthesizedExpression =
  (\mt expr -> maybe expr (`distributeComplexity` expr) mt)
    <$> optionMaybe (Term <$> anyCriteriaLiteralParser <*> acceptablePatternParser)
    <*> (spaces *> ichar '(' *> expressionParser <* spaces <* ichar ')')

setOpOperator :: Monad m => m SetOp -> m (Expression -> Expression -> Expression)
setOpOperator sop =
  flip Expression <$> sop

{-
This example below
@distributeComplexity "a" "b(c) u| (d i| e(f))" == "a(b(c)) u| (a(d) i| a(e(f)))"@
would previously be sugared as:
  "a(b(c)) u| (a(d e(f)))" but the new AST desugars this automatically and distributes
  a() over all of its predicates.
-}

{- |
 Given a term and expression, distribute the term over the expression as a new
 expression of 'ComplexTerm`s

 @distributeComplexity "a" "b u| d" == "a(b) u| a(d)"@
 @distributeComplexity "a" "b(c) u| (d i| e(f))" == "a(b(c)) u| (a(d) i| a(e(f)))"@
-}
distributeComplexity :: Term -> Expression -> Expression
distributeComplexity t expr = case expr of
  Value ti -> case ti of
    Nullary te -> Value . NAry $ t :<- Bottom te
    NAry ct -> Value . NAry $ t :<- ct
  Expression ex so ex' ->
    Expression
      (distributeComplexity t ex)
      so
      (distributeComplexity t ex')

{- |
 Parses a 'SetOp` literal or nothing.
-}
explicitOpParser :: SetOpParser
explicitOpParser = choice . map try $ [unionOpParser, intersectOpParser, diffOpParser]

unionOpParser :: SetOpParser
unionOpParser = (ichar 'u' *> ichar '|') $> Union

intersectOpParser :: SetOpParser
intersectOpParser = (ichar 'i' *> ichar '|') $> Intersect

diffOpParser :: SetOpParser
diffOpParser = (ichar 'd' *> ichar '|') $> Difference

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
descriptorCriteriaLiteralParser = (ichar 'd' *> ichar '.') $> DescriptorCriteria

metaDescriptorCriteriaLiteralParser :: QueryCriteriaLiteralParser
metaDescriptorCriteriaLiteralParser =
  (ichar 'r' *> ichar '.') $> MetaDescriptorCriteria

filePatternCriteriaLiteralParser :: QueryCriteriaLiteralParser
filePatternCriteriaLiteralParser = (ichar 'p' *> ichar '.') $> FilePatternCriteria

untaggedCriteriaLiteralParser :: QueryCriteriaLiteralParser
untaggedCriteriaLiteralParser = (ichar 'u' *> ichar '.') $> UntaggedCriteria

{- |
 Case-insensitive 'Char` parser.
-}
ichar :: Stream s m Char => Char -> ParsecT s u m Char
ichar c = satisfy (\c' -> toLower c == toLower c')

acceptablePatternParser :: Parser Text
acceptablePatternParser =
  (fmap T.pack . many1 $ acceptableCharParser)
    <?> ( "an acceptable pattern of characters not in the set, '"
            ++ charRequiringEscape
            ++ "' or any of those characters with an escape char, \\, before it."
        )

acceptableCharParser :: Parser Char
acceptableCharParser = do
  c <-
    ( try
        ( do
            void $ ichar '\\'
            oneOf charRequiringEscape
        )
        <|> ichar '\\'
      )
      <|> notDisallowedChars
  when (c `elem` ("udUD" :: String)) (notFollowedBy (ichar '|' <|> ichar '.'))
  when (c `elem` ("rpRP" :: String)) (notFollowedBy $ ichar '.')
  when (c `elem` ("iI" :: String)) (notFollowedBy $ ichar '|')
  return c

notDisallowedChars :: Parser Char
notDisallowedChars = noneOf charRequiringEscape

charRequiringEscape :: [Char]
charRequiringEscape = "().| \t\n\r"
