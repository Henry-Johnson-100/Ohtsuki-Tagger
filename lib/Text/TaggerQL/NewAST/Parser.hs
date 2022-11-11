{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.NewAST.Parser (
  expressionParser,
) where

import Control.Monad (void, when)
import Data.Char (toLower)
import Data.Tagger
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.NewAST.AST

type Parser a = Parsec Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

expressionParser :: Parser Expression
expressionParser =
  chainl1
    ( spaces
        *> ( bracketedExpressionParser
              <|> try distributedExpressionParser
              <|> ( (\qc p -> Value . Simple $ Term qc p)
                      <$> anyCriteriaLiteralParser <*> acceptablePatternParser
                  )
           )
    )
    (spaces *> setOpOperator explicitOpParser)

distributedExpressionParser :: Parser Expression
distributedExpressionParser = do
  t <- Term <$> anyCriteriaLiteralParser <*> acceptablePatternParser
  spaces
  void $ ichar '{'
  expr <- expressionParser
  spaces
  void $ ichar '}'
  return $ distributeComplexity t expr

bracketedExpressionParser :: Parser Expression
bracketedExpressionParser =
  ichar '(' *> expressionParser <* spaces <* ichar ')'

setOpOperator :: Monad m => m SetOp -> m (Expression -> Expression -> Expression)
setOpOperator sop = do
  so <- sop
  return $ \l r -> Expression l so r

{-
This example below
@distributeComplexity "a" "b{c} u| (d i| e{f})" == "a{b{c}} u| (a{d} i| a{e{f}})"@
would previously be sugared as:
  "a{b{c}} u| (a{d e{f}})" but the new AST desugars this automatically and distributes
  a{} over all of its predicates.
-}

{- |
 Given a term and expression, distribute the term over the expression as a new
 expression of 'ComplexTerm`s

 @distributeComplexity "a" "b u| d" == "a{b} u| a{d}"@
 @distributeComplexity "a" "b{c} u| (d i| e{f})" == "a{b{c}} u| (a{d} i| a{e{f}})"@
-}
distributeComplexity :: Term -> Expression -> Expression
distributeComplexity t expr = case expr of
  Value ti -> case ti of
    Simple te -> Value . Relational $ t :<- Bottom te
    Relational ct -> Value . Relational $ t :<- ct
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
unionOpParser = ichar 'u' >> ichar '|' >> return Union

intersectOpParser :: SetOpParser
intersectOpParser = ichar 'i' >> ichar '|' >> return Intersect

diffOpParser :: SetOpParser
diffOpParser = ichar 'd' >> ichar '|' >> return Difference

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
charRequiringEscape = "{}().| \t\n\r"
