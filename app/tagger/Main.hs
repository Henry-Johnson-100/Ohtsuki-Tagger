import Control.Lens
import Control.Monad.Trans.Except
import Data.Config
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Directory
import System.FilePath
import System.IO

main :: IO ()
main = do
  ec <- runExceptT getConfig
  either
    printConfigError
    runWithConfig
    ec

runWithConfig :: TaggerConfig -> IO ()
runWithConfig c = do
  workingDir <- getCurrentDirectory
  dbDir <- makeAbsolute . takeDirectory . T.unpack $ c ^. dbConf . path
  setCurrentDirectory dbDir
  getCurrentDirectory >>= putStrLn
  setCurrentDirectory workingDir
  getCurrentDirectory >>= putStrLn

printConfigError :: Text -> IO ()
printConfigError e = do
  hPutStrLn stderr "Error while parsing config file:\n"
  T.IO.hPutStrLn stderr e
  hPutStrLn stderr "\nPlease use this as a template:\n"
  hPrintConf stderr exampleConf