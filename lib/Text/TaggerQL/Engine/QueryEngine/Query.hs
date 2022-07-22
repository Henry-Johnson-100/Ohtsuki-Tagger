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

{- |
 Always returns an empty set. Also, should not even possible to occur in the first place.
-}
dSubU :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubU _ _ = return IS.empty

dSubP :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubP = (subTagQuery q .) . superSubParams
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
) as t
JOIN (
  SELECT t.subTagOfId "id"
  FROM Tag t
  JOIN File f
    ON t.fileId = f.id
  WHERE f.filePath LIKE :sub ESCAPE '\'
) as t1 USING (id)
ORDER BY t.id ASC
|]

dSubR :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubR = (subTagQuery q .) . superSubParams
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

dSubD :: Text -> Text -> QueryReaderT TagKeySet IO TagKeySet
dSubD = (subTagQuery q .) . superSubParams
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

templateSubQuery ::
  (GenericCriteria c1, GenericCriteria c2) =>
  NamedParamQuery Super c1 ->
  NamedParamQuery Sub c2 ->
  NamedParamQuery (Super, Sub) (c1, c2)
templateSubQuery superQ subQ =
  NamedParamQuery $
    partialNamedParamQuery tSelect
      `qcat` ("(" <> namedParamQuery superQ <> ") AS t")
      `qcat` "JOIN"
      `qcat` ("(" <> namedParamQuery subQ <> ") AS t1 USING (id)")
      `qcat` partialNamedParamQuery tOrder
 where
  tSelect :: PartialNamedParamQuery NoParam NoCriteria
  tSelect =
    [r|SELECT DISTINCT t.id|]
  tOrder :: PartialNamedParamQuery NoParam NoCriteria
  tOrder = [r|ORDER BY t.id ASC|]

constructSuper :: GenericCriteria c => c -> NamedParamQuery Super c
constructSuper = undefined

constructSub :: GenericCriteria c => c -> NamedParamQuery Sub c
constructSub = undefined

completeSuperQuery ::
  GenericCriteria c =>
  ( PartialNamedParamQuery (Either Super Sub) c
  , Maybe (PartialNamedParamQuery (Either Super Sub) c)
  ) ->
  NamedParamQuery Super c
completeSuperQuery
  ( partialNamedParamQuery -> x
    , fmap partialNamedParamQuery -> y
    ) =
    let body = [r|SELECT t.id FROM Tag t |] <> x <> " :super "
     in NamedParamQuery $ maybe body (qcat body) y

completeSubQuery ::
  GenericCriteria c =>
  ( PartialNamedParamQuery (Either Super Sub) c
  , Maybe (PartialNamedParamQuery (Either Super Sub) c)
  ) ->
  NamedParamQuery Sub c
completeSubQuery (partialNamedParamQuery -> x, fmap partialNamedParamQuery -> y) =
  let body = [r|SELECT t.subTagOfId "id" FROM Tag t |] <> x <> " :sub "
   in NamedParamQuery $ maybe body (qcat body) y

type Present a = Maybe a
type Absent a = Maybe a

dQuery ::
  ( PartialNamedParamQuery (Either Super Sub) D
  , Present (PartialNamedParamQuery (Either Super Sub) D)
  )
dQuery =
  ( [r|
JOIN Descriptor d 
  ON t.descriptorId = d.id 
WHERE d.descriptor LIKE|]
  , Just [r|ESCAPE '\'|]
  )

