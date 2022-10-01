{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Opt.Data (
  TaggerDBAudit (..),
  TaggerDBStats (..),
  TaggerCommand (..),
  TaggerDBCommand (..),
  TaggerEx (..),
) where

import Data.Monoid (Any)
import Data.Text (Text)
import Database.Tagger

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
  { _tcAddFile :: [Text]
  , _tcRemoveFile :: [Text]
  , _tcDeleteFile :: [Text]
  , _tcMoveFile :: [(Text, Text)]
  , _tcReportTags :: [Text]
  , _tcAddDescriptor :: [Text]
  , _tcRemoveDescriptor :: [Text]
  , _tcTagFile :: [(Text, Text)]
  , _tcRelateDescriptor :: [(Text, Text)]
  }
  deriving (Show, Eq)

instance Semigroup TaggerCommand where
  (<>) :: TaggerCommand -> TaggerCommand -> TaggerCommand
  (TaggerCommand a b c d e f g h i) <> (TaggerCommand a' b' c' d' e' f' g' h' i') =
    TaggerCommand
      (a <> a')
      (b <> b')
      (c <> c')
      (d <> d')
      (e <> e')
      (f <> f')
      (g <> g')
      (h <> h')
      (i <> i')

instance Monoid TaggerCommand where
  mempty :: TaggerCommand
  mempty =
    TaggerCommand [] [] [] [] [] [] [] [] []

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