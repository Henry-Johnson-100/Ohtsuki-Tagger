{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.Config
  ( TaggerConfig (..),
    OptAbsPath (..),
    TaggerDatabaseConfig (..),
    TaggerDatabasePathConfig (..),
    TaggerStyleConfig (..),
    TaggerStyleFontConfig (..),
    taggerConfigCodec,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Toml ((.=))
import qualified Toml

{-
[database]
  [database.path]
    directory optional
    database
    backup
    script

[style]
  [style.font]
    directory optional
    thin
    regular
    bold
-}

testDecode :: FilePath -> IO ()
testDecode p = do
  r <- Toml.decodeFileEither taggerConfigCodec p
  either
    (T.IO.putStrLn . Toml.prettyTomlDecodeErrors)
    (T.IO.putStrLn . Toml.encode taggerConfigCodec)
    r

data TaggerConfig = TaggerConfig
  { databaseConfig :: !TaggerDatabaseConfig,
    styleConfig :: !TaggerStyleConfig
  }
  deriving (Show, Eq, OptAbsPath)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.table taggerDatabaseConfigCodec "database" .= databaseConfig
    <*> Toml.table taggerStyleConfigCodec "style" .= styleConfig

data TaggerStyleConfig = TaggerStyleConfig
  { styleFontConfig :: !TaggerStyleFontConfig
  }
  deriving (Show, Eq, OptAbsPath)

taggerStyleConfigCodec :: Toml.TomlCodec TaggerStyleConfig
taggerStyleConfigCodec =
  TaggerStyleConfig
    <$> Toml.table taggerStyleFontConfigCodec "font" .= styleFontConfig

data TaggerStyleFontConfig = TaggerStyleFontConfig
  { fontDirectory :: !(Maybe T.Text),
    fontThin :: !T.Text,
    fontRegular :: !T.Text,
    fontBold :: !T.Text
  }
  deriving (Show, Eq)

instance OptAbsPath TaggerStyleFontConfig where
  getResolvedPath pf c = maybe (pf c) (T.append (pf c)) (fontDirectory c)

taggerStyleFontConfigCodec :: Toml.TomlCodec TaggerStyleFontConfig
taggerStyleFontConfigCodec =
  TaggerStyleFontConfig
    <$> Toml.dioptional (Toml.text "directory") .= fontDirectory
    <*> Toml.text "thin" .= fontThin
    <*> Toml.text "regular" .= fontRegular
    <*> Toml.text "bold" .= fontBold

data TaggerDatabaseConfig = TaggerDatabaseConfig
  { databasePathConfig :: !TaggerDatabasePathConfig
  }
  deriving (Show, Eq, OptAbsPath)

taggerDatabaseConfigCodec :: Toml.TomlCodec TaggerDatabaseConfig
taggerDatabaseConfigCodec =
  TaggerDatabaseConfig
    <$> Toml.table taggerDatabasePathConfigCodec "path" .= databasePathConfig

data TaggerDatabasePathConfig = TaggerDatabasePathConfig
  { databasePathDirectory :: !(Maybe T.Text),
    databasePathDatabase :: !T.Text,
    databasePathBackup :: !T.Text,
    databasePathScript :: !T.Text
  }
  deriving (Show, Eq)

instance OptAbsPath TaggerDatabasePathConfig where
  getResolvedPath pf c = maybe (pf c) (T.append (pf c)) (databasePathDirectory c)

taggerDatabasePathConfigCodec :: Toml.TomlCodec TaggerDatabasePathConfig
taggerDatabasePathConfigCodec =
  TaggerDatabasePathConfig
    <$> Toml.dioptional (Toml.text "directory") .= databasePathDirectory
    <*> Toml.text "database" .= databasePathDatabase
    <*> Toml.text "backup" .= databasePathBackup
    <*> Toml.text "script" .= databasePathScript

class OptAbsPath a where
  getResolvedPath :: (a -> T.Text) -> a -> T.Text
  getResolvedPath pf c = pf c