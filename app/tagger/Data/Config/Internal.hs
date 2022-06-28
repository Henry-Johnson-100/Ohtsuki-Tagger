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
  { _selectionconfDisplayParents :: Int
  , _selectionconfBufferSize :: Int
  }
  deriving (Show, Eq)

selectionConfigCodec :: Toml.Codec SelectionConfig SelectionConfig
selectionConfigCodec =
  SelectionConfig
    <$> Toml.int "display_parents" .= _selectionconfDisplayParents
    <*> Toml.int "buffer_size" .= _selectionconfBufferSize

data DescriptorTreeConfig = DescriptorTreeConfig
  { _descriptortreeconfTreeRootRequest :: Text
  }
  deriving (Show, Eq)

descriptorTreeConfigCodec :: Toml.TomlCodec DescriptorTreeConfig
descriptorTreeConfigCodec =
  DescriptorTreeConfig
    <$> Toml.text "root_request" .= _descriptortreeconfTreeRootRequest

data StyleConfig = StyleConfig
  { _styleconfFont :: FontConfig
  , _styleconfWindow :: WindowConfig
  }
  deriving (Show, Eq)

styleConfigCodec :: Toml.Codec StyleConfig StyleConfig
styleConfigCodec =
  StyleConfig
    <$> Toml.table fontConfigCodec "font" .= _styleconfFont
    <*> Toml.table windowConfigCodec "window" .= _styleconfWindow

data WindowConfig = WindowConfig
  { _windowconfMaximize :: Bool
  , _windowconfSizeX :: Integer
  , _windowconfSizeY :: Integer
  , _windowconfScalingFactor :: Double
  , _windowconfIcon :: Maybe Text
  }
  deriving (Show, Eq)

windowConfigCodec :: Toml.Codec WindowConfig WindowConfig
windowConfigCodec =
  WindowConfig
    <$> Toml.bool "maximize" .= _windowconfMaximize
    <*> Toml.integer "size_x" .= _windowconfSizeX
    <*> Toml.integer "size_y" .= _windowconfSizeY
    <*> Toml.double "scaling_factor" .= _windowconfScalingFactor
    <*> Toml.dioptional (Toml.text "icon") .= _windowconfIcon

data FontConfig = FontConfig
  { _fontconfRegular :: Maybe Text
  , _fontconfThin :: Maybe Text
  , _fontconfBold :: Maybe Text
  }
  deriving (Show, Eq)

fontConfigCodec :: Toml.Codec FontConfig FontConfig
fontConfigCodec =
  FontConfig
    <$> Toml.dioptional (Toml.text "_fontconfRegular") .= _fontconfRegular
    <*> Toml.dioptional (Toml.text "_fontconfThin") .= _fontconfThin
    <*> Toml.dioptional (Toml.text "_fontconfBold") .= _fontconfBold