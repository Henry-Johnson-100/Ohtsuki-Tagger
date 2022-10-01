import Control.Monad
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Reader
import Data.Monoid (Any (..))
import Data.Version (showVersion)
import Database.Tagger
import Opt
import Opt.Data
import Opt.Parser
import Options.Applicative
import System.Directory
import System.FilePath
import Tagger.Info

main :: IO ()
main = do
  r <- execParser taggerExParser
  runTaggerEx r

runTaggerEx :: TaggerEx -> IO ()
runTaggerEx TaggerExVersion = putStrLn . showVersion $ taggerVersion
runTaggerEx (TaggerExDB dbPath (TaggerDBCommand a s withFiles)) = do
  curDir <- getCurrentDirectory
  let dbDir = takeDirectory dbPath
  setCurrentDirectory dbDir
  conn <- open' dbPath
  flip runReaderT conn $ do
    when (getAny a) mainReportAudit
  setCurrentDirectory curDir
