{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Config (
  TaggerConfig (..),
  getOptConf,
) where

import Control.Monad (guard, (<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Text as T
import Paths_yuitagger
import System.Directory (doesFileExist)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Toml (
  TomlCodec,
  decodeFileEither,
  dioptional,
  double,
  prettyTomlDecodeErrors,
  (.=),
 )

data TaggerConfig = TaggerConfig
  { taggerConfigScaleFactor :: !(Maybe Double)
  }
  deriving (Show, Eq)

taggerConfigCodec :: TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig <$> dioptional (double "scale_factor") .= taggerConfigScaleFactor

getOptConf :: MaybeT IO TaggerConfig
getOptConf = do
  bd <- lift getBinDir
  let f = bd </> "tagger.toml"
  optConfigExists <- lift $ doesFileExist f
  guard optConfigExists
  c <- lift $ decodeFileEither taggerConfigCodec f
  either
    ( const (MaybeT . return $ Nothing)
        <=< lift . hPutStrLn stderr . T.unpack . prettyTomlDecodeErrors
    )
    return
    c
