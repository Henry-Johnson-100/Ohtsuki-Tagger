{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Opt.Data (
  TaggerDBAudit (..),
  TaggerDBStats (..),
) where

import Database.Tagger

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