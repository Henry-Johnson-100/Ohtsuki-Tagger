{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Parser.Internal (
  combinableSentenceParser,
  sentenceParser,
  termTreeParser,
  termTreeChildrenParser,
  noRelationTreeParser,
  termParser,
) where

import Control.Monad (void, when)
import Data.Char (toLower)

import Data.Maybe
import Data.Tagger (QueryCriteria (..), SetOp (..))
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.AST

newtype Token a = Token {getToken :: a} deriving (Show, Eq)

type Parser a = Parsec T.Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

-- {- |
--  Parses many 'Sentence`s to form a whole 'Request`.
-- -}
-- requestParser :: RequestParser
-- requestParser = Request <$> many1 sentenceParser

requestParser :: Parser (Request T.Text)
requestParser = do
  spaces
  r <- Request <$> many1 combinableSentenceParser
  spaces
  return r

combinableSentenceParser :: Parser (CombinableSentence T.Text)
combinableSentenceParser = do
  so <- explicitOpParser <|> pure Intersect
  spaces
  maybeParen <- optionMaybe queryOpenParser
  s <- sentenceParser
  maybe (pure ()) (const queryCloseParser) maybeParen
  return $ CombinableSentence so s

sentenceParser :: Parser (Sentence T.Text)
sentenceParser = do
  cts <- sepBy1 (try $ termTreeParser True) spaces
  return $ Sentence cts

{- |
 Parse a complete 'TermTree` including any nested children.
-}
termTreeParser ::
  -- | 'True` if the 'TermTree` is top-level.
  --This determines if simple 'Terms` are 'Simple` or 'Bottom`
  Bool ->
  Parser (TermTree T.Text)
termTreeParser isTopLevel = do
  basis <- noRelationTreeParser isTopLevel
  spaces
  predicates <- optionMaybe termTreeChildrenParser
  when
    (isJust predicates && (termCriteria . termTreeNode $ basis) == UntaggedCriteria)
    (fail "Cannot use UntaggedCriteria literal \"U.\" with a subquery.")
  spaces
  return $ maybe basis (newPredicates basis) predicates

{- |
 Parse all 'TermTree`s inside of a given subclause surrounded by {}
-}
termTreeChildrenParser :: Parser [TermTree T.Text]
termTreeChildrenParser = do
  subClauseScopeOpenParser
  t <- sepBy1 (termTreeParser False) spaces
  subClauseScopeCloseParser
  return t

{- |
 Parse a 'TermTree` without checking for children.
-}
noRelationTreeParser :: Bool -> Parser (TermTree T.Text)
noRelationTreeParser isTopLevel =
  if isTopLevel
    then Simple <$> termParser
    else Bottom <$> termParser

-- Simple <$> termParser

{- |
 Parse a 'Term` from a 'QueryCriteria` literal and text pattern.
-}
termParser :: Parser (Term T.Text)
termParser = do
  qc <- anyCriteriaLiteralParser
  p <- acceptablePatternParser
  return . Term qc . getToken $ p

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

acceptablePatternParser =
  (fmap (Token . T.pack) . many1 $ acceptableCharParser)
    <?> ( "an acceptable pattern of characters not in the set, '"
            ++ charRequiringEscape
            ++ "' or any of those characters with an escape char, \\, before it."
        )

{- |
 * Can contain any non white-space chars
 * Can contain any escaped chars
 * Stops parsing at reserved tokens and does not consume them
-}
newPatternParser =
  Token . T.pack
    <$> manyTill1
      anyChar
      ( lookAhead $
          choice
            ( try
                <$> [ void . many1 $ space
                    , void queryCriteriaLiteralReservedToken
                    , void setOpLiteralReservedToken
                    , eof
                    ]
            )
      )

-- anyEscapedCharParser :: Parser T.Text
-- anyEscapedCharParser = () <|> (T.pack <$> anyChar)

manyTill1 ::
  Stream s m t =>
  ParsecT s u m a ->
  ParsecT s u m end ->
  ParsecT s u m [a]
manyTill1 p e = do
  r <- manyTill p e
  when (null r) (fail "") <?> "one or more tokens in manyTill1"
  return r

queryCriteriaLiteralReservedToken :: Parser (Token QueryCriteria)
queryCriteriaLiteralReservedToken =
  Token
    <$> choice
      ( try
          <$> [ descriptorCriteriaLiteralParser
              , metaDescriptorCriteriaLiteralParser
              , filePatternCriteriaLiteralParser
              , untaggedCriteriaLiteralParser
              ]
      )

setOpLiteralReservedToken :: Parser (Token SetOp)
setOpLiteralReservedToken =
  Token
    <$> choice
      ( try
          <$> [ unionOpParser
              , intersectOpParser
              , diffOpParser
              ]
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

reservedTokenParser :: Parser ()
reservedTokenParser =
  choice $
    try
      <$> [ void descriptorCriteriaLiteralParser
          , void metaDescriptorCriteriaLiteralParser
          , void filePatternCriteriaLiteralParser
          , void untaggedCriteriaLiteralParser
          , void unionOpParser
          , void intersectOpParser
          , void diffOpParser
          ]