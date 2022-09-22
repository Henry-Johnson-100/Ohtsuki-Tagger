import Control.Monad.Trans.Cont (evalContT)
import Opt.Parser
import Options.Applicative

main :: IO ()
main = do
  pc <- execParser p'
  evalContT pc