{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Engine.QueryEngine.Query (
  dSubR,
  dSubD,
  dSubP,
  dSubU,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Text (Text)
import Database.Tagger.Connection
import Database.Tagger.Query.Type
import Database.Tagger.Type
import Text.RawString.QQ (r)
import Text.TaggerQL.AST
import Text.TaggerQL.Engine.QueryEngine.Type

{-
 ____
|  _ \
| | | |
| |_| |
|____/
-}

withDSuper ::
  (ToField v1, ToField a) =>
  TaggerQuery ->
  v1 ->
  a ->
  QueryReaderT TagKeySet IO TagKeySet
withDSuper q = (subTagQuery (constructQuery superDSubQuery q) .) . superSubParams

dSubP :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubP = withDSuper subPSubQuery

dSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubR = withDSuper subRSubQuery

dSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubD = withDSuper subDSubQuery

{- |
 Always returns an empty set. Also, should not even possible to occur in the first place.
-}
dSubU :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubU _ _ = return IS.empty

{-
 ____
|  _ \
| |_) |
|  _ <
|_| \_\
-}

withRSuper ::
  (ToField v1, ToField a) =>
  TaggerQuery ->
  v1 ->
  a ->
  QueryReaderT TagKeySet IO TagKeySet
withRSuper q = (subTagQuery (constructQuery superRSubQuery q) .) . superSubParams

rSubP :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubP = withRSuper subPSubQuery

rSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubR = withRSuper subRSubQuery

rSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubD = withRSuper subDSubQuery

rSubU :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubU _ _ = return IS.empty

constructQuery :: TaggerQuery -> TaggerQuery -> TaggerQuery
constructQuery super sub =
  [r|
SELECT DISTINCT t.id    
FROM (
|]
    <> super
    <> [r|
) AS t
JOIN (|]
    <> sub
    <> [r|) AS t1 USING (id)
ORDER BY t.id ASC
|]

superRSubQuery :: TaggerQuery
superRSubQuery =
  [r|
SELECT t.id
FROM Tag t
JOIN (
  WITH RECURSIVE qr (id) AS (
    SELECT id
    FROM Descriptor
    WHERE descriptor LIKE :super ESCAPE '\'
    UNION
    SELECT infraDescriptorId
    FROM MetaDescriptor md
    JOIN qr
      ON md.metaDescriptorId = qr.id
  )
  SELECT id FROM qr
) AS d
  ON t.descriptorId = d.id
  |]

superDSubQuery :: TaggerQuery
superDSubQuery =
  [r|
SELECT t.id
FROM Tag t
JOIN Descriptor d
WHERE d.descriptor LIKE :super ESCAPE '\'  
|]

subDSubQuery :: TaggerQuery
subDSubQuery =
  [r|
SELECT t.subTagOfId "id"
FROM Tag t
JOIN Descriptor d
WHERE d.descriptor LIKE :sub ESCAPE '\'
|]

subRSubQuery :: TaggerQuery
subRSubQuery =
  [r|
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
|]

subPSubQuery :: TaggerQuery
subPSubQuery =
  [r|
SELECT t.subTagOfId "id"
FROM Tag t
JOIN File f
  ON t.fileId = f.id
WHERE f.filePath LIKE :sub ESCAPE '\'
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
