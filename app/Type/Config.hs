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
-}

data TaggerConfig = TaggerConfig
  { _dbPath :: !T.Text,
    _dbBackup :: !T.Text,
    _dbInit :: !T.Text,
    _dbAutoConnect :: !Bool
  }
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.text "database.path" .= _dbPath
    <*> Toml.text "database.backup" .= _dbBackup
    <*> Toml.text "database.init" .= _dbInit
    <*> Toml.bool "database.auto_connect" .= _dbAutoConnect
