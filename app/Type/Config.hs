{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.Config
  ( TaggerConfig (..),
    taggerConfigCodec,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Toml ((.=))
import qualified Toml

{-
database.path
database.backup
database.init
-}

data TaggerConfig = TaggerConfig
  { dbPath :: !T.Text,
    dbBackup :: !T.Text,
    dbInit :: !T.Text
  }
  deriving (Show, Eq)

taggerConfigCodec :: Toml.TomlCodec TaggerConfig
taggerConfigCodec =
  TaggerConfig
    <$> Toml.text "database.path" .= dbPath
    <*> Toml.text "database.backup" .= dbBackup
    <*> Toml.text "database.init" .= dbInit
