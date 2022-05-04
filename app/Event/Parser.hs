{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Event.Parser
  ( RawSubTag (..),
    subTagQueryParser,
  )
where

import Data.Functor.Identity
import qualified Data.Text as T
import Text.Parsec

type RawSubTag = (T.Text, [T.Text])

type TextFieldParser a = ParsecT T.Text () Identity a

descriptorNameParser :: TextFieldParser T.Text
descriptorNameParser =
  fmap T.pack . many1 $
    letter
      <|> digit
      <|> oneOf "-_!@#$%^&*~=+.,<>"

spaces' :: TextFieldParser ()
spaces' = spaces <?> ""

subTagQueryParser :: TextFieldParser RawSubTag
subTagQueryParser = do
  rawDes <- descriptorNameParser
  spaces'
  char '{'
  spaces'
  subDess <- do
    many $ do
      sd'' <- descriptorNameParser
      spaces'
      return sd''
  char '}'
  spaces'
  return (rawDes, subDess)
