module Text.TaggerQL.Parser (
  parseRequest,
) where

import qualified Data.Text as T
import Text.Parsec (ParseError, parse)
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal

{- |
 Run the 'requestParser` on the given text.
-}
parseRequest :: T.Text -> Either ParseError (Request T.Text)
parseRequest = parse requestParser "TaggerQL"