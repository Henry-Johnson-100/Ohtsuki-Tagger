{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Opt.Data (
  TaggerDBAudit (..),
  TaggerDBStats (..),
  TaggerCommand (..),
  TaggerQueryCommand (..),
  TaggerDBCommand (..),
  TaggerEx (..),
) where

import Data.Monoid (Any)
import Data.Text (Text)
import Database.Tagger (Descriptor, File)

data TaggerEx
  = TaggerExVersion
  | TaggerExDB FilePath TaggerDBCommand
  deriving (Show, Eq)

data TaggerDBCommand = TaggerDBCommand
  { _tdbcAudit :: Any
  , _tdbcStats :: Any
  , _tdbcTaggerCommand :: TaggerCommand
  }
  deriving (Show, Eq)

instance Semigroup TaggerDBCommand where
  (<>) :: TaggerDBCommand -> TaggerDBCommand -> TaggerDBCommand
  (TaggerDBCommand a s c) <> (TaggerDBCommand a' s' c') =
    TaggerDBCommand (a <> a') (s <> s') (c <> c')

instance Monoid TaggerDBCommand where
  mempty :: TaggerDBCommand
  mempty = TaggerDBCommand mempty mempty mempty

data TaggerCommand = TaggerCommand
  { _tcQueryCommand :: Maybe TaggerQueryCommand
  }
  deriving (Show, Eq)

instance Semigroup TaggerCommand where
  (<>) :: TaggerCommand -> TaggerCommand -> TaggerCommand
  (TaggerCommand q) <> (TaggerCommand q') = TaggerCommand (q <> q')

instance Monoid TaggerCommand where
  mempty :: TaggerCommand
  mempty =
    TaggerCommand mempty

data TaggerQueryCommand = TaggerQueryCommand
  { _tqcQuery :: Text
  , _tqcRelative :: Any
  }
  deriving (Show, Eq)

instance Semigroup TaggerQueryCommand where
  (<>) :: TaggerQueryCommand -> TaggerQueryCommand -> TaggerQueryCommand
  (TaggerQueryCommand q r) <> (TaggerQueryCommand q' r') =
    TaggerQueryCommand (q <> q') (r <> r')

instance Monoid TaggerQueryCommand where
  mempty :: TaggerQueryCommand
  mempty = TaggerQueryCommand mempty mempty

data TaggerDBStats = TaggerDBStats
  { _taggerdbstatsNumberOfFiles :: Int
  , _taggerdbstatsNumberOfDescriptors :: Int
  , _taggerdbstatsNumberOfTags :: Int
  }
  deriving (Show, Eq)

emptyStats :: TaggerDBStats
emptyStats =
  TaggerDBStats 0 0 0

instance Semigroup TaggerDBStats where
  (<>) :: TaggerDBStats -> TaggerDBStats -> TaggerDBStats
  (TaggerDBStats a b c) <> (TaggerDBStats x y z) =
    TaggerDBStats (a + x) (b + y) (c + z)

instance Monoid TaggerDBStats where
  mempty :: TaggerDBStats
  mempty = emptyStats

{- |
 A data structure to report the results of a database
 audit.
-}
data TaggerDBAudit = TaggerDBAudit
  { _taggerdbauditMissingFiles :: [File]
  , _taggerdbauditUnusedDescriptorTrees :: [Descriptor]
  }
  deriving (Show, Eq)

emptyAudit :: TaggerDBAudit
emptyAudit = TaggerDBAudit mempty mempty

instance Semigroup TaggerDBAudit where
  (<>) :: TaggerDBAudit -> TaggerDBAudit -> TaggerDBAudit
  (TaggerDBAudit mfx udtx) <> (TaggerDBAudit mfy udty) =
    TaggerDBAudit (mfx <> mfy) (udtx <> udty)

instance Monoid TaggerDBAudit where
  mempty :: TaggerDBAudit
  mempty = emptyAudit