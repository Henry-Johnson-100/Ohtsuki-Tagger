{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Engine.QueryEngine.Query (
  queryTerms,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (asks)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Tagger (
  QueryCriteria (
    DescriptorCriteria,
    FilePatternCriteria,
    MetaDescriptorCriteria
  ),
 )
import Data.Text (Text)
import Database.Tagger.Connection (
  NamedParam (..),
  Only (Only),
  ToField,
  queryNamed,
 )
import Database.Tagger.Query.Type (TaggerQuery)
import Database.Tagger.Type (RecordKey, Tag)
import Text.RawString.QQ (r)
import Text.TaggerQL.AST (Term (Term))
import Text.TaggerQL.Engine.QueryEngine.Type (
  QueryEnv (queryEnvConn),
  QueryReaderT,
  TagKeySet,
 )

{- |
 Given two terms, run a subtag style search using their given 'QueryCriteria` and
 patterns.
-}
queryTerms :: Term Text -> Term Text -> QueryReaderT TagKeySet IO TagKeySet
queryTerms (Term qcx px) (Term qcy py) =
  dispatchQuery qcx qcy px py

dispatchQuery ::
  QueryCriteria ->
  QueryCriteria ->
  (Text -> Text -> QueryReaderT TagKeySet IO TagKeySet)
dispatchQuery x y =
  case x of
    DescriptorCriteria -> case y of
      DescriptorCriteria -> dSubD
      MetaDescriptorCriteria -> dSubR
      FilePatternCriteria -> dSubP
      _ -> uSubAnything
    MetaDescriptorCriteria -> case y of
      DescriptorCriteria -> rSubD
      MetaDescriptorCriteria -> rSubR
      FilePatternCriteria -> rSubP
      _ -> uSubAnything
    FilePatternCriteria -> case y of
      DescriptorCriteria -> pSubD
      MetaDescriptorCriteria -> pSubR
      FilePatternCriteria -> pSubP
      _ -> uSubAnything
    _ -> uSubAnything

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
 where
  superDSubQuery :: TaggerQuery
  superDSubQuery =
    [r|
SELECT t.id
FROM Tag t
JOIN Descriptor d
  ON t.descriptorId = d.id
WHERE d.descriptor LIKE :super ESCAPE '\'|]

dSubP :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubP = withDSuper subPSubQuery

dSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubR = withDSuper subRSubQuery

dSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubD = withDSuper subDSubQuery

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
 where
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
  ON t.descriptorId = d.id|]

rSubP :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubP = withRSuper subPSubQuery

rSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubR = withRSuper subRSubQuery

rSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
rSubD = withRSuper subDSubQuery

{-
 ____
|  _ \
| |_) |
|  __/
|_|
-}

withPSuper ::
  (ToField v1, ToField a) =>
  TaggerQuery ->
  v1 ->
  a ->
  QueryReaderT TagKeySet IO TagKeySet
withPSuper q = (subTagQuery (constructQuery superPSubQuery q) .) . superSubParams
 where
  superPSubQuery =
    [r|
SELECT t.id    
FROM Tag t
JOIN File f
  ON t.fileId = f.id
WHERE f.filePath LIKE :super ESCAPE '\'|]

pSubP :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
pSubP = withPSuper subPSubQuery

pSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
pSubR = withPSuper subRSubQuery

pSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
pSubD = withPSuper subDSubQuery

{-
 _   _
| | | |
| | | |
| |_| |
 \___/
-}

uSubAnything :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
uSubAnything _ _ = return IS.empty

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

subDSubQuery :: TaggerQuery
subDSubQuery =
  [r|
SELECT t.subTagOfId "id"
FROM Tag t
JOIN Descriptor d
  ON t.descriptorId = d.id
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
