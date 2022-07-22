{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Engine.QueryEngine.Type (
  QueryEnv (..),
  QueryReaderT,
  QueryReader,
  TagKeySet,
  FileKeySet,
  TaggedConnection,
  withQueryEnv,
  module Control.Monad.Trans.Reader,
  lift,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
import Data.Functor.Identity (Identity)
import Data.IntSet (IntSet)
import Database.Tagger.Type (TaggedConnection)

data QueryEnv a = QueryEnv
  { queryEnvConn :: TaggedConnection
  , queryEnv :: a
  }
  deriving (Show, Eq, Functor)

withQueryEnv :: (t -> a) -> QueryEnv t -> QueryEnv a
withQueryEnv f qe@(queryEnv -> e) = qe{queryEnv = f e}

type QueryReaderT a m b = ReaderT (QueryEnv a) m b

type QueryReader a b = QueryReaderT a Identity b

type TagKeySet = IntSet

type FileKeySet = IntSet
