{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Event.Parser
  ( PseudoDescriptor (..),
    PseudoSubTag (..),
    ParseError (..),
    parseQuery,
    pseudoDescriptorText,
  )
where

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Text.Parsec
  ( ParseError,
    ParsecT,
    char,
    many,
    many1,
    noneOf,
    parse,
    spaces,
    try,
    (<?>),
    (<|>),
  )

parseQuery :: T.Text -> Either ParseError [PseudoSubTag]
parseQuery = parse (spaces' >> pseudoQueryParser) "Query"

-- | A newtype wrapper around Data.Text.Text
newtype PseudoDescriptor = PDescriptor T.Text deriving (Show, Eq)

pseudoDescriptorText :: PseudoDescriptor -> T.Text
pseudoDescriptorText (PDescriptor t) = t

-- | (PseudoDescriptor, [PseudoDescriptor])
type PseudoSubTag = (PseudoDescriptor, [PseudoDescriptor])

type TextFieldParser a = ParsecT T.Text () Identity a

spaces' :: TextFieldParser ()
spaces' = spaces <?> ""

pseudoQueryParser :: TextFieldParser [PseudoSubTag]
pseudoQueryParser =
  many pseudoEitherParser
  where
    pseudoEitherParser :: ParsecT T.Text () Identity PseudoSubTag
    pseudoEitherParser = do
      pd <- try pseudoSubTagQueryParser <|> fmap (,[]) pseudoDescriptorParser
      spaces
      return pd

-- | Parses a descriptor
pseudoDescriptorParser :: TextFieldParser PseudoDescriptor
pseudoDescriptorParser =
  fmap (PDescriptor . T.pack) . many1 . noneOf $ "{} \t\n"

pseudoSubTagQueryParser :: TextFieldParser PseudoSubTag
pseudoSubTagQueryParser = do
  rawDes <- pseudoDescriptorParser
  spaces'
  char '{'
  spaces'
  subDess <- do
    many $ do
      sd'' <- pseudoDescriptorParser
      spaces'
      return sd''
  char '}'
  spaces'
  return (rawDes, subDess)
