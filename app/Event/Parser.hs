{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Event.Parser
  ( PseudoDescriptor (..),
    PseudoSubTag (..),
    pseudoSubTagQueryParser,
    pseudoDescriptorText,
  )
where

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Text.Parsec
  ( ParsecT,
    char,
    many,
    many1,
    noneOf,
    spaces,
    (<?>),
  )

newtype PseudoDescriptor = PDescriptor T.Text deriving (Show, Eq)

pseudoDescriptorText :: PseudoDescriptor -> T.Text
pseudoDescriptorText (PDescriptor t) = t

type PseudoSubTag = (PseudoDescriptor, [PseudoDescriptor])

type TextFieldParser a = ParsecT T.Text () Identity a

spaces' :: TextFieldParser ()
spaces' = spaces <?> ""

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
