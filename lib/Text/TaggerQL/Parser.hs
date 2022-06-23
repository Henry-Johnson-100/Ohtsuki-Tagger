module Text.TaggerQL.Parser (
  parseRequest,
) where

import qualified Data.Text as T
import Text.Parsec (ParseError, parse)
import Text.TaggerQL.AST (Request)
import Text.TaggerQL.Parser.Internal (requestParser)

{- |
 Parse a TaggerQL query and return a 'Request`.
-}
parseRequest :: T.Text -> Either ParseError (Request T.Text)
parseRequest = parse requestParser "TaggerQL"