{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.NewAST.Parser () where

import Control.Monad (void, when)
import Data.Char (toLower)
import Data.Tagger
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.TaggerQL.NewAST.AST

type Parser a = Parsec Text () a

type QueryCriteriaLiteralParser = Parser QueryCriteria

type SetOpParser = Parser SetOp

simpleTermIdentityParser :: Parser TermIdentity
simpleTermIdentityParser =
  spaces
    *> ( ( \qc p ->
            if T.null p
              then Zero
              else case qc of
                FilePatternCriteria -> if p == "%" then U else Simple $ Term qc p
                _other -> Simple $ Term qc p
         )
          <$> anyCriteriaLiteralParser
          <*> acceptablePatternParser
       )

-- I think it would be best to try to handle this expansion in the parser,
-- Adding this expansion during interpretation would confuse the simplicity of the AST
-- datatypes and may necessitate introduction of another sum type to
-- differentiate expanded vs. non-expanded complex terms.

{- |
 This parses a complex term as an expression.
 Since a complex term could contain syntactic sugar like branching queries which would
 expand into several expressions.

 e.g. \"a {b u| (c d)}\" -> \"a {b} u| a {c d}\"
-}
complexExpressionParser :: Parser Expression
complexExpressionParser = do
  spaces
  t <- Term <$> anyCriteriaLiteralParser <*> acceptablePatternParser
  spaces
  void $ ichar '{'
  contents <- undefined
  spaces
  void $ ichar '}'
  undefined

bottomTermParser :: Parser ComplexTerm
bottomTermParser =
  spaces
    *> ( fmap BottomTerm . Term
          <$> anyCriteriaLiteralParser <*> acceptablePatternParser
       )

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

acceptablePatternParser :: Parser Text
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
  when (c `elem` ("udUD" :: String)) (notFollowedBy (ichar '|' <|> ichar '.'))
  when (c `elem` ("rpRP" :: String)) (notFollowedBy $ ichar '.')
  when (c `elem` ("iI" :: String)) (notFollowedBy $ ichar '|')
  return c

notDisallowedChars :: Parser Char
notDisallowedChars = noneOf charRequiringEscape

charRequiringEscape :: [Char]
charRequiringEscape = "{}().| \t\n\r"
