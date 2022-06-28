module Data.Config (
  module Data.Config.Internal,
  module Data.Config.Lens,
  getConfig,
  getConfigPath,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, withExceptT)
import Data.Config.Internal
import Data.Config.Lens
import Data.Text (Text)
import qualified Paths_tagger
import System.FilePath ((</>))
import Toml (decodeFileEither, prettyTomlDecodeErrors)

{- |
 Returns a path to where a config should be,

 this is usually ~/.cabal/bin/tagger.toml

 but it is wherever Paths_tagger says the executable is installed.
-}
getConfigPath :: IO FilePath
getConfigPath = do
  cd <- Paths_tagger.getBinDir
  return $ cd </> "tagger.toml"

{- |
 Attempts to parse the file located at 'getConfigPath`
-}
getConfig :: ExceptT Text IO TaggerConfig
getConfig = do
  confPath <- lift getConfigPath
  conf <- decodeFileEither taggerConfigCodec confPath
  withExceptT prettyTomlDecodeErrors . except $ conf