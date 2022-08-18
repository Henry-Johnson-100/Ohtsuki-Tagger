{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

{-# HLINT ignore "Eta reduce" #-}

module Text.TaggerQL.Engine.QueryEngine.Query (
  QueryEnv (..),
  queryTerms,
  queryTerm,
  getFileSetFromTagSet,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
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
  ToField,
  query,
  queryNamed,
 )
import Database.Tagger.Query.Type (TaggerQuery)
import Database.Tagger.Type (File, Tag (tagId))
import Text.RawString.QQ (r)
import Text.TaggerQL.AST (Term (Term))
import Text.TaggerQL.Engine.QueryEngine.Type

{- |
 Given two terms, run a subtag style search using their given 'QueryCriteria` and
 patterns.
-}
queryTerms :: Term Text -> Term Text -> ReaderT QueryEnv IO (HashSet SubTag)
queryTerms (Term qcx px) (Term qcy py) =
  dispatchQuery qcx qcy px py

dispatchQuery ::
  QueryCriteria ->
  QueryCriteria ->
  (Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag))
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

{- |
 Run a query on a single term and produce an environment with corresponding 'Tag`s

 Naturally, querying with 'UntaggedCriteria` will always produce an empty set.
-}
queryTerm :: Term Text -> ReaderT QueryEnv IO (HashSet Tag)
queryTerm (Term qc p) = dispatchSimpleQuery qc p
 where
  dispatchSimpleQuery :: QueryCriteria -> Text -> ReaderT QueryEnv IO (HashSet Tag)
  dispatchSimpleQuery qc' p' =
    case qc' of
      DescriptorCriteria ->
        runSimpleDispatchQuery
          p'
          [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId        
FROM Tag t
JOIN Descriptor d ON t.descriptorId = d.id
WHERE d.descriptor LIKE ? ESCAPE '\'|]
      MetaDescriptorCriteria ->
        runSimpleDispatchQuery
          p'
          [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN (
  WITH RECURSIVE r(id) AS (
    SELECT id
    FROM Descriptor
    WHERE descriptor LIKE ? ESCAPE '\'
    UNION
    SELECT infraDescriptorId
    FROM MetaDescriptor md
    JOIN r ON md.metaDescriptorId = r.id
  )
  SELECT id FROM r
) AS d ON t.descriptorId = d.id|]
      FilePatternCriteria ->
        runSimpleDispatchQuery
          p'
          [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN File f ON t.fileId = f.id
WHERE f.filePath LIKE ? ESCAPE '\'|]
      _ -> return mempty
   where
    runSimpleDispatchQuery :: Text -> TaggerQuery -> ReaderT QueryEnv IO (HashSet Tag)
    runSimpleDispatchQuery p'' q'' = do
      conn <- asks queryEnvConn
      results <- lift $ query conn q'' [p''] :: ReaderT QueryEnv IO [Tag]
      return . HS.fromList $ results

getFileSetFromTagSet :: IsTag t => HashSet t -> ReaderT QueryEnv IO (HashSet File)
getFileSetFromTagSet (map (tagId . toTag) . HS.toList -> ts) = do
  conn <- asks queryEnvConn
  results <-
    lift $
      mapM (query conn q . (: [])) ts ::
      ReaderT QueryEnv IO [[File]]
  return . HS.unions . map HS.fromList $ results
 where
  q =
    [r|
SELECT
  f.id
  ,f.filePath
FROM File f
JOIN Tag t ON f.id = t.fileId
WHERE t.id = ?
    |]

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
  ReaderT QueryEnv IO (HashSet SubTag)
withDSuper q = (subTagQuery (constructQuery superDSubQuery q) .) . superSubParams
 where
  superDSubQuery :: TaggerQuery
  superDSubQuery =
    [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN Descriptor d ON t.descriptorId = d.id
WHERE d.descriptor LIKE :super ESCAPE '\'|]

dSubP :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
dSubP = withDSuper subPSubQuery

dSubR :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
dSubR = withDSuper subRSubQuery

dSubD :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
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
  ReaderT QueryEnv IO (HashSet SubTag)
withRSuper q = (subTagQuery (constructQuery superRSubQuery q) .) . superSubParams
 where
  superRSubQuery :: TaggerQuery
  superRSubQuery =
    [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN (
  WITH RECURSIVE qr (id) AS (
    SELECT id
    FROM Descriptor
    WHERE descriptor LIKE :super ESCAPE '\'
    UNION
    SELECT infraDescriptorId
    FROM MetaDescriptor md
    JOIN qr ON md.metaDescriptorId = qr.id
  )
  SELECT id FROM qr
) AS d ON t.descriptorId = d.id|]

rSubP :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
rSubP = withRSuper subPSubQuery

rSubR :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
rSubR = withRSuper subRSubQuery

rSubD :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
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
  ReaderT QueryEnv IO (HashSet SubTag)
withPSuper q = (subTagQuery (constructQuery superPSubQuery q) .) . superSubParams
 where
  superPSubQuery =
    [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN File f ON t.fileId = f.id
WHERE f.filePath LIKE :super ESCAPE '\'|]

pSubP :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
pSubP = withPSuper subPSubQuery

pSubR :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
pSubR = withPSuper subRSubQuery

pSubD :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
pSubD = withPSuper subDSubQuery

{-
 _   _
| | | |
| | | |
| |_| |
 \___/
-}

uSubAnything :: Text -> Text -> ReaderT QueryEnv IO (HashSet SubTag)
uSubAnything _ _ = return mempty

constructQuery :: TaggerQuery -> TaggerQuery -> TaggerQuery
constructQuery super sub =
  [r|
SELECT
  t1.id
  ,t1.fileId
  ,t1.descriptorId
  ,t1.subTagOfId
FROM (
|]
    <> super
    <> [r|
) AS t
JOIN (|]
    <> sub
    <> [r|) AS t1 ON t.id = t1.subTagOfId
|]

subDSubQuery :: TaggerQuery
subDSubQuery =
  [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN Descriptor d ON t.descriptorId = d.id
WHERE d.descriptor LIKE :sub ESCAPE '\'
|]

subRSubQuery :: TaggerQuery
subRSubQuery =
  [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN (
  WITH RECURSIVE qr (id) AS (
    SELECT id
    FROM Descriptor
    WHERE descriptor LIKE :sub ESCAPE '\'
    UNION
    SELECT infraDescriptorId
    FROM MetaDescriptor md
    JOIN qr ON md.metaDescriptorId = qr.id
  )
  SELECT id FROM qr
) AS d ON t.descriptorId = d.id
|]

subPSubQuery :: TaggerQuery
subPSubQuery =
  [r|
SELECT
  t.id
  ,t.fileId
  ,t.descriptorId
  ,t.subTagOfId
FROM Tag t
JOIN File f ON t.fileId = f.id
WHERE f.filePath LIKE :sub ESCAPE '\'
|]

subTagQuery ::
  TaggerQuery ->
  [NamedParam] ->
  ReaderT QueryEnv IO (HashSet SubTag)
subTagQuery q params = do
  conn <- asks queryEnvConn
  results <-
    lift $ queryNamed conn q params ::
      ReaderT QueryEnv IO [Tag]
  return . HS.fromList $ (SubTag <$> results)

superSubParams :: (ToField v1, ToField v2) => v1 -> v2 -> [NamedParam]
superSubParams super sub = [":super" := super, ":sub" := sub]
