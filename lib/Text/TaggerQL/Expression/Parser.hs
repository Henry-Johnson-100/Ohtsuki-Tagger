{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Expression.Parser (
  parseExpr,
  parseTagExpr,
  ParseError,

  -- * For Testing
  parse,

  -- ** Expression Parsers
  expressionParser,

  -- ** SubExpression Parsers
  subExpressionParser,

  -- ** Term Parsers
  tagTermParser,
  fileTermParser,

  -- ** Misc
  patternParser,
) where

import Control.Applicative ((<**>))
import Data.Char (toLower, toUpper)
import Data.Functor (($>), (<&>))
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (
  ParseError,
  Parsec,
  anyChar,
  between,
  chainl1,
  char,
  many1,
  noneOf,
  parse,
  space,
  spaces,
  try,
  (<|>),
 )
import Text.TaggerQL.Expression.AST (
  Expression (..),
  FileTerm (..),
  SubExpression (..),
  TagTerm (..),
 )

type Parser a = Parsec Text () a

parseExpr :: Text -> Either ParseError Expression
parseExpr = parse expressionParser "TaggerQL"

parseTagExpr :: Text -> Either ParseError SubExpression
parseTagExpr = parse subExpressionParser "TaggerQL"

expressionParser :: Parser Expression
expressionParser =
  spaces
    *> ( try (chainl1 lhsExprParser (flip Binary <$> setOpParser))
          <|> lhsExprParser
       )
 where
  lhsExprParser :: Parser Expression
  lhsExprParser =
    spaces
      *> ( precedentExpressionParser
            <|> ( try untaggedConstParser
                    <|> try fileTermValueParser
                    <|> ( tagTermParser
                            <**> ( tagExpressionLookAhead
                                    <|> pure TagTermValue
                                 )
                        )
                )
         )
   where
    precedentExpressionParser =
      between (char '(') (spaces *> char ')') expressionParser
    tagExpressionLookAhead =
      flip TagExpression
        <$> between
          (try (spaces *> char '{'))
          (spaces *> char '}')
          subExpressionParser

untaggedConstParser :: Parser Expression
untaggedConstParser = ichar 'u' *> char '.' $> UntaggedConst

fileTermValueParser :: Parser Expression
fileTermValueParser = FileTermValue <$> fileTermParser

subExpressionParser :: Parser SubExpression
subExpressionParser =
  spaces
    *> ( try
          ( chainl1
              lhsSubExpressionParser
              (flip SubBinary <$> setOpParser)
          )
          <|> lhsSubExpressionParser
       )
 where
  lhsSubExpressionParser :: Parser SubExpression
  lhsSubExpressionParser =
    spaces
      *> ( precedentSubExpressionParser
            <|> ( tagTermParser
                    <**> ( subExpressionLookAheadParser
                            <|> pure SubTag
                         )
                )
         )
   where
    precedentSubExpressionParser =
      between
        (char '(')
        (spaces *> char ')')
        subExpressionParser
    subExpressionLookAheadParser =
      flip SubExpression
        <$> between
          (try (spaces *> char '{'))
          (spaces *> char '}')
          subExpressionParser

fileTermParser :: Parser FileTerm
fileTermParser = ichar 'p' *> char '.' *> patternParser <&> FileTerm

tagTermParser :: Parser TagTerm
tagTermParser =
  ( ( try (ichar 'r' *> char '.' $> MetaDescriptorTerm)
        <|> try (ichar 'd' *> char '.' $> DescriptorTerm)
    )
      <|> pure MetaDescriptorTerm
  )
    <*> patternParser

patternParser :: Parser Text
patternParser =
  T.pack <$> many1 ((char '\\' *> anyChar) <|> notRestricted)

notRestricted :: Parser Char
notRestricted = noneOf restrictedChars

setOpParser :: Parser SetOp
setOpParser =
  try (spaces *> explicitSetOpParser)
    <|> (space $> Intersect)

explicitSetOpParser :: Parser SetOp
explicitSetOpParser = unionParser <|> intersectParser <|> differenceParser
 where
  unionParser = char '|' $> Union
  intersectParser = char '&' $> Intersect
  differenceParser = char '!' $> Difference

ichar :: Char -> Parser Char
ichar c = char (toUpper c) <|> char (toLower c)

restrictedChars :: [Char]
restrictedChars = "(){}!&|. \r\n"