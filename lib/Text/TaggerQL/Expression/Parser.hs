{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Expression.Parser (
  parseExpr,
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
import Control.Monad (void)
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
  eof,
  lookAhead,
  many1,
  noneOf,
  notFollowedBy,
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

expressionParser :: Parser Expression
expressionParser =
  spaces
    *> ( try (chainl1 lhsExprParser (flip Binary <$> setOpParser))
          <|> lhsExprParser
       )

lhsExprParser :: Parser Expression
lhsExprParser =
  spaces
    *> ( between (char '(') (spaces *> char ')') expressionParser
          <|> ( try untaggedConstParser <|> try fileTermValueParser
                  <|> ( tagTermParser
                          <**> ( ( flip TagExpression
                                    <$> between
                                      (try (spaces *> char '{'))
                                      (spaces *> char '}')
                                      subExpressionParser
                                 )
                                  <|> pure TagTermValue
                               )
                      )
              )
       )

untaggedConstParser :: Parser Expression
untaggedConstParser = ichar 'u' *> char '.' $> UntaggedConst

fileTermValueParser :: Parser Expression
fileTermValueParser = FileTermValue <$> fileTermParser

tagTermValueParser :: Parser Expression
tagTermValueParser = TagTermValue <$> tagTermParser

binaryParser :: Parser Expression
binaryParser = chainl1 expressionParser (flip Binary <$> setOpParser)

subExpressionParser :: Parser SubExpression
subExpressionParser = lhsSubExpressionParser

lhsSubExpressionParser :: Parser SubExpression
lhsSubExpressionParser =
  spaces
    *> ( ( tagTermParser
            <**> ( spaces
                    *> ( subExpressionOperator
                          <|> ( (\so rhs lht -> SubBinary (SubTag lht) so rhs)
                                  <$> setOpParser <*> subExpressionParser
                              )
                          <|> simpleSubtag
                       )
                 )
         )
          <|> subBinaryParser
       )
 where
  subExpressionOperator =
    flip SubExpression
      <$> between
        (char '{')
        (spaces *> char '}')
        subExpressionParser
  simpleSubtag = pure SubTag

subTagParser :: Parser SubExpression
subTagParser = SubTag <$> tagTermParser

subBinaryParser :: Parser SubExpression
subBinaryParser =
  chainl1
    (between (char '(') (spaces *> char ')') subExpressionParser <|> subTagParser)
    (flip SubBinary <$> setOpParser)

subExpressionSubParser :: Parser SubExpression
subExpressionSubParser =
  SubExpression <$> tagTermParser
    <*> ( spaces
            *> between
              (char '{')
              (spaces *> char '}')
              subExpressionParser
        )

tagExpressionParser :: Parser Expression
tagExpressionParser =
  TagExpression <$> tagTermParser
    <*> ( spaces
            *> between
              (char '{')
              (spaces *> char '}')
              subExpressionParser
        )

fileTermParser :: Parser FileTerm
fileTermParser = ichar 'p' *> char '.' *> patternParser <&> FileTerm

tagTermParser :: Parser TagTerm
tagTermParser =
  ( ( try (ichar 'r' *> char '.' $> MetaDescriptorTerm)
        <|> (ichar 'd' *> char '.' $> DescriptorTerm)
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