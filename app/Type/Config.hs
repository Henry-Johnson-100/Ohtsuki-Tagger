{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.Config
  ( TaggerConfig (..),
    DatabaseConfig (..),
    SelectionConfig (..),
    DescriptorTreeConfig (..),
    StyleConfig (..),
    FontConfig (..),
    taggerConfigCodec,
    databaseConfigCodec,
    selectionConfigCodec,
  )
where

import qualified Data.Text as T
import Toml ((.=))
import qualified Toml

{-
shell_command = ""

[database]
  path = ""
  backup = ""
  init = ""
  auto_connect = false

[selection]
  display_parents = 3
  buffer_size = 25

[descriptor_tree]
  main_request = "#META#"

[style]
  [style.font]
    regular = "/usr/local/share/fonts/i/iosevka_light.ttf"
    thin = "/usr/local/share/fonts/i/iosevka_thin.ttf"
    bold = "/usr/local/share/fonts/i/iosevka_bold.ttf"
-}

data TaggerConfig = TaggerConfig
  { _dbconf :: !DatabaseConfig,
    _selectionconf :: !SelectionConfig,
    _descriptorTreeConf :: !DescriptorTreeConfig,
    _styleConf :: !StyleConfig,
    _shellCmd :: !T.Text
  }
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.table databaseConfigCodec "database" .= _dbconf
    <*> Toml.table selectionConfigCodec "selection" .= _selectionconf
    <*> Toml.table descriptorTreeConfigCodec "descriptor_tree" .= _descriptorTreeConf
    <*> Toml.table styleConfigCodec "style" .= _styleConf
    <*> Toml.text "shell_command" .= _shellCmd

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
  { _selectionDisplayParents :: !Int,
    _selectionBufferSize :: !Int
  }
  deriving (Show, Eq)

selectionConfigCodec :: Toml.Codec SelectionConfig SelectionConfig
selectionConfigCodec =
  SelectionConfig
    <$> Toml.int "display_parents" .= _selectionDisplayParents
    <*> Toml.int "buffer_size" .= _selectionBufferSize

data DescriptorTreeConfig = DescriptorTreeConfig
  { _descriptorTreeMainRequest :: !T.Text
  }
  deriving (Show, Eq)

descriptorTreeConfigCodec :: Toml.TomlCodec DescriptorTreeConfig
descriptorTreeConfigCodec =
  DescriptorTreeConfig
    <$> Toml.text "main_request" .= _descriptorTreeMainRequest

data StyleConfig = StyleConfig
  { font :: !FontConfig
  }
  deriving (Show, Eq)

styleConfigCodec :: Toml.Codec StyleConfig StyleConfig
styleConfigCodec =
  StyleConfig
    <$> Toml.table fontConfigCodec "font" .= font

data FontConfig = FontConfig
  { regular :: !T.Text,
    thin :: !T.Text,
    bold :: !T.Text
  }
  deriving (Show, Eq)

fontConfigCodec :: Toml.Codec FontConfig FontConfig
fontConfigCodec =
  FontConfig
    <$> Toml.text "regular" .= regular
    <*> Toml.text "thin" .= thin
    <*> Toml.text "bold" .= bold