{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.Parser
Description : Parsers for the expression-based TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
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
  ParsecT,
  Stream,
  anyChar,
  between,
  char,
  many1,
  noneOf,
  optionMaybe,
  parse,
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
    *> ( try
          ( myChainl1
              lhsExprParser
              (flip Binary <$> (spaces *> setOpParser))
              pure
          )
          <|> lhsExprParser
       )
 where
  lhsExprParser :: Parser Expression
  lhsExprParser =
    spaces
      *> ( precedentExpressionParser
            <|> ( try fileTermValueParser
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

fileTermValueParser :: Parser Expression
fileTermValueParser = FileTermValue <$> fileTermParser

subExpressionParser :: Parser SubExpression
subExpressionParser =
  spaces
    *> ( try
          ( myChainl1
              lhsSubExpressionParser
              (flip SubBinary <$> (spaces *> setOpParser))
              pure
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

{-# INLINEABLE myChainl1 #-}

{- |
 @myChainl1 p op defP@ defines a left associative application of the @op@ operator parser
  to values of @p@.

  If the right hand side of an application fails to parse,
    then @defP@ is applied to a value of type @p@,
    to produce a parser for just that value.

  This parser is used for left-associative operations whose left-hand-side may or may not
  be applied to a right-hand-side and where the operator may be implicit, making
  lookahead prohibitive.
-}
myChainl1 ::
  Stream s m t =>
  ParsecT s u m a ->
  ParsecT s u m (a -> a -> a) ->
  (a -> ParsecT s u m a) ->
  ParsecT s u m a
myChainl1 p op defP = do
  x <- p
  rest x
 where
  rest x =
    ( do
        f <- op
        y <- try . optionMaybe $ p
        maybe (defP x) (rest . f x) y
    )
      <|> return x

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
setOpParser = explicitSetOpParser <|> pure Intersect

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