{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

{- |
Module      : Text.TaggerQL.Expression.Parser
Description : Parsers for the expression-based TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.Parser (
  parseExpr,
  parseTagExpr,
  parseQueryExpression,
  parseTagExpression,
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
  termPatternParser,
) where

import Control.Applicative ((<**>))
import Control.Monad (join, unless)
import Data.Char (toLower, toUpper)
import Data.Functor (($>), (<&>))
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.Expression.AST

type Parser a = Parsec Text () a

{- |
 Parse an 'Expression` from a TaggerQL query.
-}
parseExpr :: Text -> Either ParseError Expression
parseExpr = parse expressionParser "TaggerQL"

{- |
 Parse a 'SubExpression` from a TaggerQL query.

 Used to parse expressions that tag images, rather than query.
-}
parseTagExpr :: Text -> Either ParseError SubExpression
parseTagExpr = parse subExpressionParser "TaggerQL"

expressionParser :: Parser Expression
expressionParser =
  spaces
    *> ( try
          ( myChainl1
              lhsExprParser
              ( (\so lhs rhs -> BinaryExpression (BinaryOperation lhs so rhs))
                  <$> (spaces *> setOpParser)
              )
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
      (\se tt -> TagExpression (TagTermExtension tt se))
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
              ( (\so lhs rhs -> BinarySubExpression (BinaryOperation lhs so rhs))
                  <$> (spaces *> setOpParser)
              )
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
      (\se tt -> SubExpression (TagTermExtension tt se))
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

termPatternParser :: Parser Pattern
termPatternParser = Pattern <$> patternParser

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

parseQueryExpression :: Text -> Either ParseError QueryExpression
parseQueryExpression =
  parse
    allInput
    "TaggerQL"
 where
  allInput =
    queryExpressionParser
      <* spaces
      <* failIfNotConsumed

parseTagExpression :: Text -> Either ParseError (TagExpression (DTerm Pattern))
parseTagExpression =
  parse
    allInput
    "TaggerQL - Tag"
 where
  allInput =
    tagExpressionParser
      <* spaces
      <* failIfNotConsumed

failIfNotConsumed :: Parser ()
failIfNotConsumed = do
  remains <- getInput
  unless (T.null remains) . fail $
    "Failed to consume all text, remainder: \""
      <> T.unpack remains
      <> "\""

queryExpressionParser :: Parser QueryExpression
queryExpressionParser =
  spaces
    *> ( fmap (QueryExpression . join)
          . ringExprParser
          . fmap runQueryExpression
          $ queryLeafParser
       )

fileLeafParser :: Parser QueryLeaf
fileLeafParser =
  spaces
    *> (FileLeaf <$> (ichar 'p' *> char '.' *> termPatternParser))

tagExpressionParser :: Parser (TagExpression (DTerm Pattern))
tagExpressionParser =
  spaces
    *> ( fmap TagRing
          . ringExprParser
          . fmap runTagExpressionTerm
          $ (spaces *> tagExpressionTermParser)
       )

dTermParser :: Parser (DTerm Pattern)
dTermParser =
  spaces
    *> (dTermConstructorParser <*> termPatternParser)

newtype MagmaOperand = MagmaOperand {runMagmaOperand :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

magmaOperandParser :: Parser MagmaOperand
magmaOperandParser =
  spaces
    *> ( MagmaOperand
          <$> between
            (char '{')
            (char '}')
            (spaces *> tagExpressionParser <* spaces)
       )

newtype MinimalTagExpressionTerm = MinimalTagExpressionTerm
  {runMinimalTagExpressionTerm :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

minimalTagExpressionTermParser :: Parser MinimalTagExpressionTerm
minimalTagExpressionTermParser =
  spaces
    *> ( MinimalTagExpressionTerm
          <$> ( (TagValue <$> dTermParser)
                  <**> ( try applyAsMagma
                          <|> pure id
                       )
              )
       )
 where
  applyAsMagma =
    ( \(MagmaOperand inner) outerDTerm ->
        TagMagma $ Magma outerDTerm :$ inner
    )
      <$> magmaOperandParser

newtype TagExpressionTerm = TagExpressionTerm
  {runTagExpressionTerm :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

tagExpressionTermParser :: Parser TagExpressionTerm
tagExpressionTermParser =
  spaces
    *> ( TagExpressionTerm
          <$> ( parenthesizedTagExpression
                  <|> (runMinimalTagExpressionTerm <$> minimalTagExpressionTermParser)
              )
       )
 where
  parenthesizedTagExpression =
    parenthesized tagExpressionParser
      <**> ( try
              ( (\(MagmaOperand o) outerTE -> TagMagma $ Magma outerTE :$ o)
                  <$> magmaOperandParser
              )
              <|> pure id
           )

newtype DistributiveTagLeaf = DistributeTagLeaf
  {runDistributiveTagLeaf :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

distributiveTagLeafParser :: Parser DistributiveTagLeaf
distributiveTagLeafParser =
  spaces
    *> ( DistributeTagLeaf
          <$> ( parenthesized tagExpressionParser
                  <**> ( (\(MagmaOperand o) outerTE -> TagMagma $ Magma outerTE :$ o)
                          <$> magmaOperandParser
                       )
              )
       )

queryLeafParser :: Parser QueryExpression
queryLeafParser =
  spaces
    *> (
         -- Parenthesized parsers
         ( try distributiveTagLeaf
            <|> parenthesizedQueryExpression
         )
          <|>
          -- Non parenthesized
          ( try fileLeaf
              <|> minimalTagLeaf
          )
       )
 where
  fileLeaf = QueryExpression . Ring <$> fileLeafParser
  minimalTagLeaf =
    QueryExpression
      . Ring
      . TagLeaf
      . runMinimalTagExpressionTerm
      <$> minimalTagExpressionTermParser
  distributiveTagLeaf =
    QueryExpression
      . Ring
      . TagLeaf
      . runDistributiveTagLeaf
      <$> distributiveTagLeafParser
  parenthesizedQueryExpression = parenthesized queryExpressionParser

parenthesized :: Parser a -> Parser a
parenthesized = between (spaces *> char '(') (spaces *> char ')')

dTermConstructorParser :: Parser (a -> DTerm a)
dTermConstructorParser =
  ( try (ichar 'r' *> char '.' $> DMetaTerm)
      <|> try (ichar 'd' *> char '.' $> DTerm)
  )
    <|> pure DMetaTerm

ringExprParser :: Parser a -> Parser (RingExpression a)
ringExprParser termP =
  myChainl1
    (pure <$> termP)
    ringExprConstructorParser
    pure

ringExprConstructorParser ::
  Parser
    (RingExpression a -> RingExpression a -> RingExpression a)
ringExprConstructorParser =
  ( \so ->
      case so of
        Union -> (:+)
        Intersect -> (:*)
        Difference -> (:-)
  )
    <$> (spaces *> setOpParser)