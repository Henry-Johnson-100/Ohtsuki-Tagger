{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Parser.Internal (
  -- * Parsers
  requestParser,
  sentenceTreeParser,
  sentenceParser,
  complexTermParser,
  complexTermChildrenParser,
  bottomTermParser,
  termParser,

  -- * Additional
  parse,

  -- ** 'SetOp` Literals
  -- $SetOp

  -- ** 'QueryCriteria` Literals
  -- $QueryCriteria
) where

import Control.Monad (void, when)
import Data.Char (toLower)

import Data.Maybe (isJust)
import Data.Tagger (QueryCriteria (..), SetOp (..))
import qualified Data.Text as T
import Text.Parsec (
  Parsec,
  ParsecT,
  Stream,
  between,
  choice,
  many1,
  noneOf,
  notFollowedBy,
  oneOf,
  optionMaybe,
  parse,
  satisfy,
  sepBy1,
  spaces,
  try,
  (<?>),
  (<|>),
 )
import Text.TaggerQL.AST

type Parser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

{- $SetOp
 A 'SetOp` is a data type that describes how data structures can be combined together.

A 'SetOp` literal is TaggerQL's representation of this type's constructors.
Any reference to either a 'SetOp` or 'SetOp` Literal probably refers to these:

* u| == 'Union`
* i| == 'Intersect`
* d| == 'Difference`

Note that 'SetOp` Literals are case-insensitive.
-}

{- $QueryCriteria
  A 'QueryCriteria` is a data type that determines what a search is performed on.

TaggerQL's representation of its constructors as literals are as follows:

 * d. == 'DescriptorCriteria`
 * r. == 'MetaDescriptorCriteria` \*
 * p. == 'FilePatternCriteria`
 * u. == 'UntaggedCriteria`

 Note that the literals are case-insensitive.

 \* this literal is 'r.' to refer to a "relation"
-}

{- |
 Parse many 'SentenceTree`s and store them in a list.

 'SentenceTree`s will later be subject to querying and a left-associative fold
 to combine their results, so order of sentences in this parser is important.
-}
requestParser :: Parser (Request T.Text)
requestParser = Request <$> between spaces spaces (many1 sentenceTreeParser)

{- |
 Parse a 'SentenceTree` which is either:

 * A 'Sentence` preceded by an optional 'SetOp` literal, defaults to 'Union`
 * An explicit 'SetOp` literal and 1 or more 'SentenceTree`s wrapped in ().
 The 'SentenceTree` parser is recursive and can nest arbitrary amounts of times.
-}
sentenceTreeParser :: Parser (SentenceTree T.Text)
sentenceTreeParser =
  try sentenceTreeBranchParser
    <|> sentenceTreeNodeParser

sentenceTreeBranchParser :: Parser (SentenceTree T.Text)
sentenceTreeBranchParser = do
  so <- explicitOpParser
  spaces
  queryOpenParser
  cs <- sepBy1 sentenceTreeParser spaces
  queryCloseParser
  spaces
  return $ SentenceBranch so cs

sentenceTreeNodeParser :: Parser (SentenceTree T.Text)
sentenceTreeNodeParser = SentenceNode <$> combinableSentenceParser

combinableSentenceParser :: Parser (SentenceSet T.Text)
combinableSentenceParser = do
  -- is it ever possible to get this default SetOp value from a query?
  -- -- yes, "a d| (b u| c) d e f" produces this default value in the SentenceNode
  -- -- containing [d, e, f] so that:
  -- -- SentenceNode (SentenceSet {DefaultValue} (Sentence [d, e, f]))
  so <- explicitOpParser <|> pure Intersect
  spaces
  s <- sentenceParser
  return $ SentenceSet so s

{- |
 Parses 1 or more 'TermTree`s that are separated by spaces.

 This parser parses a list of 'ComplexTerm`s and lifts them to a 'TermTree` before
 mapping over the list and calling 'simplifyTermTree` to convert all of the top-level
 'Bottom`s to 'SimpleTerm`s.
-}
sentenceParser :: Parser (Sentence T.Text)
sentenceParser = do
  cts <- map (simplifyTermTree . Complex) <$> sepBy1 (try complexTermParser) spaces
  return . Sentence $ cts

{- |
 Parse a 'ComplexTerm` with 0 or more nested 'ComplexTerm` predicates.

 Defaults to a 'Bottom` if there are no predicates.
-}
complexTermParser :: Parser (ComplexTerm T.Text)
complexTermParser = do
  basis <- bottomTermParser
  spaces
  predicates <- optionMaybe complexTermChildrenParser
  when
    (isJust predicates && (termCriteria . complexTermNode $ basis) == UntaggedCriteria)
    (fail "Cannot use UntaggedCriteria literal \"U.\" with a subquery.")
  spaces
  let node = maybe basis (newPredicates basis) predicates
  return node

{- |
 Parse all 'ComplexTerm`s inside of a given subclause surrounded by {}
-}
complexTermChildrenParser :: Parser [ComplexTerm T.Text]
complexTermChildrenParser = do
  subClauseScopeOpenParser
  t <- sepBy1 complexTermParser spaces
  subClauseScopeCloseParser
  return t

{- |
 Parse a 'Term` and return it as 'Bottom`.
-}
bottomTermParser :: Parser (ComplexTerm T.Text)
bottomTermParser = Bottom <$> termParser

{- |
 Parse a 'Term` from a 'QueryCriteria` literal and text pattern.

 The pattern may not contain members of the set 'charRequiringEscape` except if
  an escape character '\' is placed before one.
-}
termParser :: Parser (Term T.Text)
termParser = do
  qc <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  return $ Term qc p

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

queryOpenParser :: Parser ()
queryOpenParser = void (ichar '(') >> spaces

queryCloseParser :: Parser ()
queryCloseParser = void spaces >> void (ichar ')')

subClauseScopeOpenParser :: Parser ()
subClauseScopeOpenParser = void (ichar '{') >> spaces

subClauseScopeCloseParser :: Parser ()
subClauseScopeCloseParser = spaces >> void (ichar '}')

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

acceptablePatternParser :: Parser T.Text
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
  when (c `elem` ("ud" :: String)) (notFollowedBy (ichar '|' <|> ichar '.'))
  when (c `elem` ("rp" :: String)) (notFollowedBy $ ichar '.')
  when (c == 'i') (notFollowedBy $ ichar '|')
  return c

notDisallowedChars :: Parser Char
notDisallowedChars = noneOf charRequiringEscape

charRequiringEscape :: [Char]
charRequiringEscape = "{}().| \t\n\r"
