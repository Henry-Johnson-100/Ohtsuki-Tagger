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

import Control.Applicative
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

queryDSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
queryDSubR = (subTagQuery q .) . superSubParams
 where
  q =
    [r|
SELECT DISTINCT t.id
FROM (
  SELECT t.id
  FROM Tag t
  JOIN Descriptor d
    ON t.descriptorId = d.id
  WHERE d.descriptor LIKE :super ESCAPE '\'
) AS t
JOIN (
  SELECT t.subTagOfId "id"
  FROM Tag t
  JOIN (
    WITH RECURSIVE qr (id) AS (
      SELECT id
      FROM Descriptor
      WHERE descriptor LIKE :sub ESCAPE '\'
      UNION
      SELECT infraDescriptorId
      FROM MetaDescriptor md
      JOIN qr
        ON md.metaDescriptorId = qr.id
    )
    SELECT id FROM qr
  ) AS d
    ON t.descriptorId = d.id
) AS t1 USING (id)
ORDER BY t.id ASC
|]

queryDSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
queryDSubD = (subTagQuery q .) . superSubParams
 where
  q =
    [r|
SELECT DISTINCT t.id    
FROM (
  SELECT t.id
  FROM Tag t
  JOIN Descriptor d
  WHERE d.descriptor LIKE :super ESCAPE '\'
) AS t
JOIN (
  SELECT t.subTagOfId "id"
  FROM Tag t
  JOIN Descriptor d
  WHERE d.descriptor LIKE :sub ESCAPE '\'
) AS t1 USING (id)
ORDER BY t.id ASC
|]

subTagQuery ::
  TaggerQuery ->
  [NamedParam] ->
  QueryReaderT TagKeySet IO TagKeySet
subTagQuery q params =
  asks queryEnvConn
    >>= (\c -> fromDistinctAscResultList <$> lift (queryNamed c q params))

fromDistinctAscResultList :: [Only (RecordKey Tag)] -> IntSet
fromDistinctAscResultList = IS.fromDistinctAscList . map (\(Only k) -> fromIntegral k)

superSubParams :: (ToField v1, ToField v2) => v1 -> v2 -> [NamedParam]
superSubParams super sub = [":super" := super, ":sub" := sub]
