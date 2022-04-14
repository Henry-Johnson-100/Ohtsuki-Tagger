{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Type.Config
  ( TaggerConfig (..),
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
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.table taggerDatabaseConfigCodec "database" .= databaseConfig
    <*> Toml.table taggerStyleConfigCodec "style" .= styleConfig

data TaggerStyleConfig = TaggerStyleConfig
  { styleFontConfig :: !TaggerStyleFontConfig
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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

taggerDatabasePathConfigCodec :: Toml.TomlCodec TaggerDatabasePathConfig
taggerDatabasePathConfigCodec =
  TaggerDatabasePathConfig
    <$> Toml.dioptional (Toml.text "directory") .= databasePathDirectory
    <*> Toml.text "database" .= databasePathDatabase
    <*> Toml.text "backup" .= databasePathBackup
    <*> Toml.text "script" .= databasePathScript
