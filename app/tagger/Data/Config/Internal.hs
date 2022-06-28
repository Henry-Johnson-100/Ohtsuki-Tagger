{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Config.Internal (
  TaggerConfig (..),
  DatabaseConfig (..),
  SelectionConfig (..),
  DescriptorTreeConfig (..),
  StyleConfig (..),
  FontConfig (..),
  WindowConfig (..),
  taggerConfigCodec,
) where

import Data.Text (Text)
import Toml ((.=))
import qualified Toml

data TaggerConfig = TaggerConfig
  { _taggerconfDbConf :: DatabaseConfig
  , _taggerconfSelectionConf :: SelectionConfig
  , _taggerconfDescriptorTreeConf :: DescriptorTreeConfig
  , _taggerconfStyleConf :: StyleConfig
  , _taggerconfShellCmd :: Text
  }
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.table databaseConfigCodec "database" .= _taggerconfDbConf
    <*> Toml.table selectionConfigCodec "selection" .= _taggerconfSelectionConf
    <*> Toml.table descriptorTreeConfigCodec "descriptor_tree" .= _taggerconfDescriptorTreeConf
    <*> Toml.table styleConfigCodec "style" .= _taggerconfStyleConf
    <*> Toml.text "shell_command" .= _taggerconfShellCmd

data DatabaseConfig = DatabaseConfig
  { _dbconfPath :: Text
  , _dbconfInMemory :: Bool
  }
  deriving (Show, Eq)

databaseConfigCodec :: Toml.TomlCodec DatabaseConfig
databaseConfigCodec =
  DatabaseConfig
    <$> Toml.text "path" .= _dbconfPath
    <*> Toml.bool "in_memory" .= _dbconfInMemory

data SelectionConfig = SelectionConfig
  { _selectionDisplayParents :: Int
  , _selectionBufferSize :: Int
  }
  deriving (Show, Eq)

selectionConfigCodec :: Toml.Codec SelectionConfig SelectionConfig
selectionConfigCodec =
  SelectionConfig
    <$> Toml.int "display_parents" .= _selectionDisplayParents
    <*> Toml.int "buffer_size" .= _selectionBufferSize

data DescriptorTreeConfig = DescriptorTreeConfig
  { _descriptorTreeRootRequest :: Text
  }
  deriving (Show, Eq)

descriptorTreeConfigCodec :: Toml.TomlCodec DescriptorTreeConfig
descriptorTreeConfigCodec =
  DescriptorTreeConfig
    <$> Toml.text "root_request" .= _descriptorTreeRootRequest

data StyleConfig = StyleConfig
  { font :: FontConfig
  , window :: WindowConfig
  }
  deriving (Show, Eq)

styleConfigCodec :: Toml.Codec StyleConfig StyleConfig
styleConfigCodec =
  StyleConfig
    <$> Toml.table fontConfigCodec "font" .= font
    <*> Toml.table windowConfigCodec "window" .= window

data WindowConfig = WindowConfig
  { maximize :: Bool
  , windowSizeX :: Integer
  , windowSizeY :: Integer
  , windowScalingFactor :: Double
  , windowIcon :: Maybe Text
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
  { regular :: Maybe Text
  , thin :: Maybe Text
  , bold :: Maybe Text
  }
  deriving (Show, Eq)

fontConfigCodec :: Toml.Codec FontConfig FontConfig
fontConfigCodec =
  FontConfig
    <$> Toml.dioptional (Toml.text "regular") .= regular
    <*> Toml.dioptional (Toml.text "thin") .= thin
    <*> Toml.dioptional (Toml.text "bold") .= bold