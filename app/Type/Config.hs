{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.Config
  ( TaggerConfig (..),
    taggerConfigCodec,
  )
where

import qualified Data.Text as T
import Toml ((.=))
import qualified Toml

{-
database.path = ""
database.backup = ""
database.init = ""
database.auto_connect = false
selection.display_parents = 3
-}

data TaggerConfig = TaggerConfig
  { _dbPath :: !T.Text,
    _dbBackup :: !T.Text,
    _dbInit :: !T.Text,
    _dbAutoConnect :: !Bool,
    _selectionDisplayParents :: !Int
  }
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.text "database.path" .= _dbPath
    <*> Toml.text "database.backup" .= _dbBackup
    <*> Toml.text "database.init" .= _dbInit
    <*> Toml.bool "database.auto_connect" .= _dbAutoConnect
    <*> Toml.int "selection.display_parents" .= _selectionDisplayParents
