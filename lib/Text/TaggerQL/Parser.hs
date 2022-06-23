module Text.TaggerQL.Parser (
  parseRequest,
  printfTaggerQL,
) where

import qualified Data.Text as T
import Text.Parsec (ParseError, parse)
import Text.TaggerQL.AST (Request, formatRequest)
import Text.TaggerQL.Parser.Internal (requestParser)

{- |
 Parse a TaggerQL query and return a 'Request`.
-}
parseRequest :: T.Text -> Either ParseError (Request T.Text)
parseRequest = parse requestParser "TaggerQL"

{- |
 Given a raw TaggerQL query, print a formatted representation of it.
-}
printfTaggerQL :: T.Text -> IO ()
printfTaggerQL =
  either print (putStrLn . formatRequest)
    . parse requestParser "TaggerQL Test"