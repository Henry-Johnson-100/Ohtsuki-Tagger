{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.Config
  ( TaggerConfig (..),
    DatabaseConfig (..),
    SelectionConfig (..),
    taggerConfigCodec,
    databaseConfigCodec,
    selectionConfigCodec,
  )
where

import qualified Data.Text as T
import Toml ((.=))
import qualified Toml

{-
[database]
  path = ""
  backup = ""
  init = ""
  auto_connect = false

[selection]
  display_parents = 3
-}

data TaggerConfig = TaggerConfig
  { _dbconf :: !DatabaseConfig,
    _selectionconf :: !SelectionConfig
  }
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.table databaseConfigCodec "database" .= _dbconf
    <*> Toml.table selectionConfigCodec "selection" .= _selectionconf

data DatabaseConfig = DatabaseConfig
  { _dbconfPath :: !T.Text,
    _dbconfBackup :: !T.Text,
    _dbconfInit :: !T.Text,
    _dbconfAutoConnect :: !Bool
  }
  deriving (Show, Eq)

databaseConfigCodec :: Toml.TomlCodec DatabaseConfig
databaseConfigCodec =
  DatabaseConfig
    <$> Toml.text "path" .= _dbconfPath
    <*> Toml.text "backup" .= _dbconfBackup
    <*> Toml.text "init" .= _dbconfInit
    <*> Toml.bool "auto_connect" .= _dbconfAutoConnect

data SelectionConfig = SelectionConfig
  { _selectionDisplayParents :: !Int
  }
  deriving (Show, Eq)

selectionConfigCodec :: Toml.Codec SelectionConfig SelectionConfig
selectionConfigCodec =
  SelectionConfig <$> Toml.int "display_parents" .= _selectionDisplayParents