{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

{- |
Module      : Text.TaggerQL.Engine.Query
Description : This module houses the internal workings of the TaggerQL's query engine.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Engine.Query () where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity (Identity)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Text (Text)
import Database.Tagger.Connection
import Database.Tagger.Query.Type
import Database.Tagger.Type
import Text.RawString.QQ (r)
import Text.TaggerQL.AST

data QueryEnv a = QueryEnv
  { queryEnvConn :: TaggedConnection
  , queryEnv :: a
  }
  deriving (Show, Eq, Functor)

type QueryReaderT a m b = ReaderT (QueryEnv a) m b

type QueryReader a b = QueryReaderT a Identity b

type TagKeySet = IntSet

fromDistinctAscResultList :: [Only (RecordKey Tag)] -> IntSet
fromDistinctAscResultList = IS.fromDistinctAscList . map (\(Only k) -> fromIntegral k)

queryDSubD ::
  Text ->
  Text ->
  QueryReaderT TagKeySet IO TagKeySet
queryDSubD super sub = do
  c <- asks queryEnvConn
  results <-
    lift $ queryNamed c q [":super" := super, ":sub" := sub] ::
      QueryReaderT TagKeySet IO [Only (RecordKey Tag)]
  return . fromDistinctAscResultList $ results
 where
  q =
    [r|
SELECT DISTINCT t.id
FROM Tag t
JOIN Tag t1
  ON t.id = t1.subTagOfId
JOIN Descriptor d
  ON t.descriptorId = d.id
JOIN Descriptor d1
  ON t1.descriptorId = d1.id
WHERE d.descriptor LIKE :super ESCAPE '\'
  AND d1.descriptor LIKE :sub ESCAPE '\'
ORDER BY t.id ASC|]