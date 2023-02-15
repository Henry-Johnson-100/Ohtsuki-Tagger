{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.Parser
Description : Parsers for the expression-based TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.Parser (
  parseQueryExpression,
  parseTagExpression,
  ParseError,

  -- * For Testing
  parse,

  -- ** Misc
  patternTextParser,
  patternParser,
) where

import Control.Monad (unless)
import Data.Char (toLower, toUpper)
import Data.Functor (($>))
import qualified Data.List as L
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
  getInput,
  many,
  many1,
  noneOf,
  optionMaybe,
  parse,
  spaces,
  try,
  (<|>),
 )
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.AST.Editor ((<-#))

type Parser a = Parsec Text () a

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

patternTextParser :: Parser Text
patternTextParser =
  T.pack <$> many1 (char '\\' *> anyChar <|> notRestricted)

patternParser :: Parser Pattern
patternParser = Pattern <$> patternTextParser

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

restrictedChars :: String
restrictedChars = "(){}!&|. \r\n"

foldBracketedTags :: Magma b => b -> Maybe b -> b
foldBracketedTags overTerm = maybe overTerm (overTerm ∙)

parseQueryExpression :: Text -> Either ParseError FreeQueryExpression
parseQueryExpression =
  parse
    allInput
    "TaggerQL"
 where
  allInput =
    queryExpressionParser
      <* spaces
      <* failIfNotConsumed

parseTagExpression :: Text -> Either ParseError TagQueryExpression
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

queryExpressionParser :: Parser FreeQueryExpression
queryExpressionParser =
  spaces
    *> ( fmap (FreeQueryExpression . (>>= runFreeQueryExpression))
          . ringExprParser
          . fmap runQueryTerm
          $ queryTermParser
       )

tagExpressionParser :: Parser TagQueryExpression
tagExpressionParser =
  spaces
    *> ( T
          <$> ringExprParser
            ( runTagTerm
                <$> tagTermParser
            )
       )

filePathParser :: Parser (Either Pattern TagQueryExpression)
filePathParser =
  spaces
    *> (Left <$> (ichar 'p' *> char '.' *> patternParser))

descriptorPatternParser :: Parser (DTerm Pattern)
descriptorPatternParser =
  spaces
    *> (dTermConstructorParser <*> patternParser)

newtype MinimalTagExpression = MinimalTagExpression
  {runMinTagExpr :: TagQueryExpression}
  deriving (Show, Eq, Rng, Magma)

minimalTagExpressionParser :: Parser MinimalTagExpression
minimalTagExpressionParser =
  spaces
    *> fmap
      MinimalTagExpression
      ( foldBracketedTags
          <$> fmap pure descriptorPatternParser
          <*> (fmap runBracketTag <$> zeroOrManyBracketedTagParser)
      )

newtype ParenthesizedTag = ParenthesizedTag
  {runParenTag :: TagQueryExpression}
  deriving (Show, Eq, Rng, Magma)

parenthesizedTagParser :: Parser ParenthesizedTag
parenthesizedTagParser = ParenthesizedTag <$> parenthesized tagExpressionParser

newtype BracketedTag = BracketedTag
  {runBracketTag :: TagQueryExpression}
  deriving (Show, Eq, Rng, Magma)

bracketedTagParser :: Parser BracketedTag
bracketedTagParser =
  BracketedTag
    <$> between (char '{') (spaces *> char '}') tagExpressionParser

{- |
 Parses zero or many 'BracketedTag` then folds them together.
-}
zeroOrManyBracketedTagParser :: Parser (Maybe BracketedTag)
zeroOrManyBracketedTagParser =
  fmap
    ( \xs -> case xs of
        [] -> Nothing
        _notNull -> Just $ L.foldl1' (∙) xs
    )
    (many . try $ spaces *> bracketedTagParser)

newtype TagTerm = TagTerm
  {runTagTerm :: TagQueryExpression}
  deriving (Show, Eq, Rng, Magma)

tagTermParser :: Parser TagTerm
tagTermParser =
  spaces
    *> ( bracketedTagTerm
          <|> parenthesizedTagTerm
          <|> minimalTagTerm
       )
 where
  bracketedTagTerm =
    TagTerm . runBracketTag
      <$> ( foldBracketedTags
              <$> bracketedTagParser
                <*> zeroOrManyBracketedTagParser
          )
  parenthesizedTagTerm =
    TagTerm
      <$> ( foldBracketedTags
              <$> fmap runParenTag parenthesizedTagParser
                <*> (fmap runBracketTag <$> zeroOrManyBracketedTagParser)
          )
  minimalTagTerm = TagTerm . runMinTagExpr <$> minimalTagExpressionParser

newtype ParenthesizedQuery = ParenthesizedQuery
  {runParenQuery :: FreeQueryExpression}
  deriving (Show, Eq, Rng)

parenthesizedQueryParser :: Parser ParenthesizedQuery
parenthesizedQueryParser =
  ParenthesizedQuery <$> parenthesized queryExpressionParser

newtype QueryTerm = QueryTerm {runQueryTerm :: FreeQueryExpression}
  deriving (Show, Eq, Rng)

queryTermParser :: Parser QueryTerm
queryTermParser =
  spaces
    *> ( bracketedQuery
          <|> parenthesizedQuery
          <|> try filePathTerm
          <|> minimalTagQuery
       )
 where
  parenthesizedQuery =
    QueryTerm
      <$> ( ( \(ParenthesizedQuery q) ->
                maybe q ((q <-#) . runBracketTag) ::
                  Maybe BracketedTag -> FreeQueryExpression
            )
              <$> parenthesizedQueryParser <*> zeroOrManyBracketedTagParser
          )
  bracketedQuery =
    QueryTerm
      . liftSimpleQueryRing
      . Node
      . Right
      . runBracketTag
      <$> (foldBracketedTags <$> bracketedTagParser <*> zeroOrManyBracketedTagParser)
  minimalTagQuery =
    QueryTerm
      . liftSimpleQueryRing
      . Node
      . Right
      . runMinTagExpr
      <$> minimalTagExpressionParser
  filePathTerm =
    QueryTerm
      . liftSimpleQueryRing
      . Node
      <$> filePathParser

parenthesized :: Parser a -> Parser a
parenthesized = between (char '(') (spaces *> char ')')

dTermConstructorParser :: Parser (a -> DTerm a)
dTermConstructorParser =
  try (ichar 'r' *> char '.' $> DMetaTerm)
    <|> try (ichar 'd' *> char '.' $> DTerm)
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
        Union -> (+.)
        Intersect -> (*.)
        Difference -> (-.)
  )
    <$> (spaces *> setOpParser)