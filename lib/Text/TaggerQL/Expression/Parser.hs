{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

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
  parseFExpr,
  parseTExpr,

  -- * For Testing
  parse,

  -- ** Expression Parsers
  expressionParser,

  -- ** SubExpression Parsers
  subExpressionParser,

  -- ** Term Parsers
  tagTermParser,
  fileTermParser,
  fValueParser,
  dTermPatternParser,

  -- ** Misc
  patternParser,
  patternParser',
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
import Text.TaggerQL.Expression.AST

type Parser a = Parsec Text () a

{- |
 Parse an 'Expression` from a TaggerQL query.
-}
parseExpr :: Text -> Either ParseError Expression
parseExpr = parse expressionParser "TaggerQL"

parseFExpr :: Text -> Either ParseError (FExpression (DTerm Pattern) Pattern)
parseFExpr = parse fExpressionParser "TaggerQL"

parseTExpr :: Text -> Either ParseError (TExpression (DTerm Pattern))
parseTExpr = parse tExpressionParser "TaggerQL"

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

fExpressionParser :: Parser (FExpression (DTerm Pattern) Pattern)
fExpressionParser =
  spaces
    *> ( try
          ( myChainl1
              lhsFExpressionParser
              (dispatchLng <$> (spaces *> setOpParser))
              pure
          )
          <|> lhsFExpressionParser
       )

lhsFExpressionParser :: Parser (FExpression (DTerm Pattern) Pattern)
lhsFExpressionParser =
  spaces
    *> ( try (fmap FValue fValueParser)
          <|> liftTExpressionParser
          <|> between (char '(') (spaces *> char ')') fExpressionParser
       )

{- |
 A lifting function that is distributive over BinaryTExpressions
-}
liftBinaryTExpression :: TExpression a -> FExpression a b
liftBinaryTExpression texpr = case texpr of
  BinaryTExpression bo -> BinaryFExpression . fmap LiftTExpression $ bo
  _simpleLift -> LiftTExpression texpr

liftTExpressionParser :: Parser (FExpression (DTerm Pattern) Pattern)
liftTExpressionParser =
  liftDTerm
    <|> liftBinaryTExpression
      <$> try
        ( between
            (char '(')
            (spaces *> char ')')
            tExpressionParser
            <**> reverseApplicationTExpressionParser
        )
 where
  liftDTerm =
    LiftTExpression
      <$> ( fmap TValue dTermPatternParser
              <**> (reverseApplicationTExpressionParser <|> pure id)
          )

lhsTExpressionParser :: Parser (TExpression (DTerm Pattern))
lhsTExpressionParser =
  lhsTExpressionParserNotApplied
    <**> ( reverseApplicationTExpressionParser
            <|> pure id
         )

tExpressionParser :: Parser (TExpression (DTerm Pattern))
tExpressionParser =
  spaces
    *> ( try
          ( myChainl1
              lhsTExpressionParser
              (dispatchLng <$> (spaces *> setOpParser))
              pure
          )
          <|> lhsTExpressionParser
       )

{- |
 A lhs TExpression parser that does not check for right application
-}
lhsTExpressionParserNotApplied :: Parser (TExpression (DTerm Pattern))
lhsTExpressionParserNotApplied =
  spaces
    *> ( (try fValueParser *> fail "")
          <|> between (char '(') (spaces *> char ')') tExpressionParser
          <|> fmap TValue dTermPatternParser
       )

reverseApplicationTExpressionParser ::
  Parser
    (TExpression (DTerm Pattern) -> TExpression (DTerm Pattern))
reverseApplicationTExpressionParser =
  flip (@>)
    <$> between
      (try (spaces *> char '{'))
      (spaces *> char '}')
      tExpressionParser

fValueParser :: Parser Pattern
fValueParser = ichar 'p' *> char '.' *> patternParser'

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

dTermPatternParser :: Parser (DTerm Pattern)
dTermPatternParser =
  ( ( try (ichar 'r' *> char '.' $> DMetaTerm)
        <|> try (ichar 'd' *> char '.' $> DTerm)
    )
      <|> pure DMetaTerm
  )
    <*> patternParser'

patternParser' :: Parser Pattern
patternParser' = Pattern <$> patternParser

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