{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ViewPatterns #-}
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

import Control.Applicative ((<**>))
import Control.Monad (join, unless)
import Data.Char (toLower, toUpper)
import Data.Functor (($>))
import qualified Data.List as L
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
import Text.TaggerQL.Expression.AST (
  DTerm (..),
  Magma ((#)),
  MagmaExpression (Magma, (:$)),
  Pattern (..),
  QueryExpression (..),
  QueryLeaf (..),
  RingExpression (..),
  Rng ((*.)),
  TagExpression (..),
 )

type Parser a = Parsec Text () a

{- |
 A function handling left-distribution of a 'TagExpression` into a 'QueryExpression`.

 Where a 'FileLeaf` becomes an intersection and a 'TagLeaf` is subject to normal
 distribution.

 Meant to operate over queries of the form:

 > (a){b}
-}
distributeTagExpression ::
  QueryExpression ->
  TagExpression (DTerm Pattern) ->
  QueryExpression
distributeTagExpression (QueryExpression qe) te =
  QueryExpression $ qe >>= distributeUnderQueryLeaf te
 where
  distributeUnderQueryLeaf ::
    TagExpression (DTerm Pattern) ->
    QueryLeaf ->
    RingExpression QueryLeaf
  distributeUnderQueryLeaf te' ql = case ql of
    FileLeaf _ -> Ring ql *. (Ring . TagLeaf $ te')
    TagLeaf te'' -> Ring . TagLeaf $ te'' # te'

infixl 6 <-#

{- |
 Infix synonym for 'distributeTagExpression`.
-}
(<-#) :: QueryExpression -> TagExpression (DTerm Pattern) -> QueryExpression
(<-#) = distributeTagExpression

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
  T.pack <$> many1 ((char '\\' *> anyChar) <|> notRestricted)

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

restrictedChars :: [Char]
restrictedChars = "(){}!&|. \r\n"

foldBracketedTags :: Magma m => m -> [m] -> m
foldBracketedTags overTerm [] = overTerm
foldBracketedTags overTerm xs = overTerm # L.foldl1' (#) xs

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
    *> ( fmap (QueryExpression . (>>= runQueryExpression))
          . ringExprParser
          . fmap runQueryTerm
          $ queryTermParser
       )

tagExpressionParser :: Parser (TagExpression (DTerm Pattern))
tagExpressionParser =
  spaces
    *> (TagRing <$> ringExprParser (runTagTerm <$> tagTermParser))

filePathParser :: Parser QueryLeaf
filePathParser =
  spaces
    *> (FileLeaf <$> (ichar 'p' *> char '.' *> patternParser))

descriptorPatternParser :: Parser (DTerm Pattern)
descriptorPatternParser =
  spaces
    *> (dTermConstructorParser <*> patternParser)

newtype MinimalTagExpression = MinimalTagExpression
  {runMinTagExpr :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

minimalTagExpressionParser :: Parser MinimalTagExpression
minimalTagExpressionParser =
  spaces
    *> fmap
      MinimalTagExpression
      ( foldBracketedTags
          <$> fmap pure descriptorPatternParser
          <*> fmap (map runBracketTag) (many bracketedTagParser)
      )

newtype ParenthesizedTag = ParenthesizedTag
  {runParenTag :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

parenthesizedTagParser :: Parser ParenthesizedTag
parenthesizedTagParser = ParenthesizedTag <$> parenthesized tagExpressionParser

newtype BracketedTag = BracketedTag
  {runBracketTag :: TagExpression (DTerm Pattern)}
  deriving (Show, Eq, Rng, Magma)

bracketedTagParser :: Parser BracketedTag
bracketedTagParser =
  BracketedTag
    <$> between (char '{') (spaces *> char '}') tagExpressionParser

newtype TagTerm = TagTerm
  {runTagTerm :: TagExpression (DTerm Pattern)}
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
                <*> many bracketedTagParser
          )
  parenthesizedTagTerm =
    TagTerm
      <$> ( foldBracketedTags
              <$> fmap runParenTag parenthesizedTagParser
                <*> fmap (map runBracketTag) (many bracketedTagParser)
          )
  minimalTagTerm = TagTerm . runMinTagExpr <$> minimalTagExpressionParser

newtype ParenthesizedQuery = ParenthesizedQuery
  {runParenQuery :: QueryExpression}
  deriving (Show, Eq, Rng)

parenthesizedQueryParser :: Parser ParenthesizedQuery
parenthesizedQueryParser =
  ParenthesizedQuery <$> parenthesized queryExpressionParser

newtype QueryTerm = QueryTerm {runQueryTerm :: QueryExpression}
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
      <$> ( ( \(ParenthesizedQuery q) (map runBracketTag -> brs) ->
                ( case brs of
                    [] -> q
                    _notNull -> q <-# L.foldl1' (#) brs
                ) ::
                  QueryExpression
            )
              <$> parenthesizedQueryParser <*> many bracketedTagParser
          )
  bracketedQuery =
    QueryTerm
      . QueryExpression
      . Ring
      . TagLeaf
      . runBracketTag
      <$> (foldBracketedTags <$> bracketedTagParser <*> many bracketedTagParser)
  minimalTagQuery =
    QueryTerm
      . QueryExpression
      . Ring
      . TagLeaf
      . runMinTagExpr
      <$> minimalTagExpressionParser
  filePathTerm =
    QueryTerm
      . QueryExpression
      . Ring
      <$> filePathParser

parenthesized :: Parser a -> Parser a
parenthesized = between (char '(') (spaces *> char ')')

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