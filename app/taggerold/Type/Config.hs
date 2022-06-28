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
    WindowConfig (..),
    taggerConfigCodec,
    databaseConfigCodec,
    selectionConfigCodec,
  )
where

import qualified Data.Text as T
import Toml ((.=))
import qualified Toml

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
    _dbconfAutoConnect :: !Bool,
    _dbconfInMemory :: !Bool
  }
  deriving (Show, Eq)

databaseConfigCodec :: Toml.TomlCodec DatabaseConfig
databaseConfigCodec =
  DatabaseConfig
    <$> Toml.text "path" .= _dbconfPath
    <*> Toml.bool "auto_connect" .= _dbconfAutoConnect
    <*> Toml.bool "in_memory" .= _dbconfInMemory

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
  { font :: !FontConfig,
    window :: !WindowConfig
  }
  deriving (Show, Eq)

styleConfigCodec :: Toml.Codec StyleConfig StyleConfig
styleConfigCodec =
  StyleConfig
    <$> Toml.table fontConfigCodec "font" .= font
    <*> Toml.table windowConfigCodec "window" .= window

data WindowConfig = WindowConfig
  { maximize :: !Bool,
    windowSizeX :: !Integer,
    windowSizeY :: !Integer,
    windowScalingFactor :: !Double,
    windowIcon :: !(Maybe T.Text)
  }
  deriving (Show, Eq)

windowConfigCodec :: Toml.Codec WindowConfig WindowConfig
windowConfigCodec =
  WindowConfig
    <$> Toml.bool "maximize" .= maximize
    <*> Toml.integer "window_size_x" .= windowSizeX
    <*> Toml.integer "window_size_y" .= windowSizeY
    <*> Toml.double "window_scaling_factor" .= windowScalingFactor
    <*> Toml.dioptional (Toml.text "window_icon") .= windowIcon

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