{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.Config (
  module Data.Config.Internal,
  module Data.Config.Lens,
  getConfig,
  getConfigPath,
  exampleConf,
  hPrintConf,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, throwE, withExceptT)
import Data.Config.Internal
import Data.Config.Lens
import Data.Text (Text, pack)
import qualified Data.Text.IO as T.IO
import qualified Paths_tagger
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (Handle)
import Toml (decodeFileEither, encode, prettyTomlDecodeErrors)

{- |
 Returns a path to where a config should be,

 this is usually ~/.cabal/bin/tagger.toml

 but it is wherever Paths_tagger says the executable is installed.
-}
getConfigPath :: ExceptT Text IO FilePath
getConfigPath = do
  cd <- lift Paths_tagger.getBinDir
  let cPath = cd </> "tagger.toml"
  fileExists <- lift $ doesFileExist cPath
  if fileExists
    then return cPath
    else throwE ("No such file exists: " <> pack cPath)

{- |
 Attempts to parse the file located at 'getConfigPath`
-}
getConfig :: ExceptT Text IO TaggerConfig
getConfig = do
  confPath <- getConfigPath
  conf <- decodeFileEither taggerConfigCodec confPath
  withExceptT prettyTomlDecodeErrors . except $ conf

hPrintConf :: Handle -> TaggerConfig -> IO ()
hPrintConf h = T.IO.hPutStrLn h . encode taggerConfigCodec

{- |
 TaggerConfig with placeholder values, to print to the screen or
 copy to a file and fill out.
-}
exampleConf :: TaggerConfig
exampleConf =
  TaggerConfig
    ( DatabaseConfig
        "path/to/database/file"
        False
    )
    ( SelectionConfig
        3
        50
    )
    ( DescriptorTreeConfig
        "#ALL#"
    )
    ( StyleConfig
        ( FontConfig
            (Just "optional/path/to/regular/font")
            (Just "optional/path/to/thin/font")
            (Just "optional/path/to/bold/font")
        )
        ( WindowConfig
            False
            1200
            800
            1.65
        )
    )
    "feh -zx. -Bwhite -g800x800 -A \";\"'echo %f'"