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
import Data.Functor.Identity (Identity (Identity))
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
  BinaryExpression (BinaryExpression),
  BinarySubExpression (BinarySubExpression),
  Expression (..),
  ExpressionLeaf (FileTermValue, TagTermValue),
  FileTerm (..),
  SubExpression (..),
  SubExpressionExtension (SubExpressionExtension),
  TagTerm (..),
  TagTermExtension (..),
 )

type Parser a = Parsec Text () a

{- |
 Parse an 'Expression` from a TaggerQL query.
-}
parseExpr :: Text -> Either ParseError (Expression Identity Identity)
parseExpr = parse expressionParser "TaggerQL"

{- |
 Parse a 'SubExpression` from a TaggerQL query.

 Used to parse expressions that tag images, rather than query.
-}
parseTagExpr :: Text -> Either ParseError (SubExpression Identity)
parseTagExpr = parse subExpressionParser "TaggerQL"

expressionParser :: Parser (Expression Identity Identity)
expressionParser =
  spaces
    *> ( try
          ( myChainl1
              lhsExprParser
              ( ( \so lhs rhs ->
                    BinaryExpressionValue
                      ( Identity
                          (BinaryExpression lhs so rhs)
                      )
                )
                  <$> (spaces *> setOpParser)
              )
              pure
          )
          <|> lhsExprParser
       )
 where
  lhsExprParser :: Parser (Expression Identity Identity)
  lhsExprParser =
    spaces
      *> ( precedentExpressionParser
            <|> ( try fileTermValueParser
                    <|> ( tagTermParser
                            <**> ( tagExpressionLookAhead
                                    <|> pure (ExpressionLeaf . Identity . TagTermValue)
                                 )
                        )
                )
         )
   where
    precedentExpressionParser =
      between (char '(') (spaces *> char ')') expressionParser
    tagExpressionLookAhead =
      ( \subExpr tt ->
          ExpressionTagTermExtension
            . Identity
            . TagTermExtension tt
            $ subExpr
      )
        <$> between
          (try (spaces *> char '{'))
          (spaces *> char '}')
          subExpressionParser

fileTermValueParser :: Parser (Expression Identity Identity)
fileTermValueParser = ExpressionLeaf . Identity . FileTermValue <$> fileTermParser

subExpressionParser :: Parser (SubExpression Identity)
subExpressionParser =
  spaces
    *> ( try
          ( myChainl1
              lhsSubExpressionParser
              ( (\so lhs rhs -> SubBinary . Identity $ BinarySubExpression lhs so rhs)
                  <$> (spaces *> setOpParser)
              )
              pure
          )
          <|> lhsSubExpressionParser
       )
 where
  lhsSubExpressionParser :: Parser (SubExpression Identity)
  lhsSubExpressionParser =
    spaces
      *> ( precedentSubExpressionParser
            <|> ( tagTermParser
                    <**> ( subExpressionLookAheadParser
                            <|> pure (SubTag . Identity)
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
      (\y tt -> SubExpression . Identity $ SubExpressionExtension tt y)
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