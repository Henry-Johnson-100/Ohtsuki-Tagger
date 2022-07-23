{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Engine.QueryEngine.Type (
  QueryEnv (..),
  QueryReader,
  TaggedConnection,
  module Control.Monad.Trans.Reader,
  lift,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
import Data.HashSet (HashSet)
import Database.Tagger.Type (Tag, TaggedConnection)

data QueryEnv = QueryEnv
  { envTagSet :: HashSet Tag
  , envConn :: TaggedConnection
  }
  deriving (Show, Eq)

type QueryReader a = ReaderT QueryEnv IO a
