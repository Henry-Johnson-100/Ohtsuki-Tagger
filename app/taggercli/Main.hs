import Opt.Parser
import Options.Applicative

main :: IO ()
main = do
  programOpts <- execParser opts
  print programOpts