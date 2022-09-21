import Opt.Parser
import Options.Applicative
import Control.Monad

main :: IO ()
main = do
  join $ execParser p'