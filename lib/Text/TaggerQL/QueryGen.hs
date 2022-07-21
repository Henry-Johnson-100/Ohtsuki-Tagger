{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Redundant case" #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.QueryGen (
  TaggerQLGenQuery,
  generateAndRunQuery,
) where

import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (maybeToList)
import Data.Tagger
import Data.Text (Text)
import Database.Tagger.Connection
import Database.Tagger.Query.Type
import Database.Tagger.Type (File, TaggedConnection)
import Text.RawString.QQ
import Text.TaggerQL.AST

data TaggerQLGenQuery = TaggerQLGenQuery
  { taggerQLGenQueryParam :: Maybe Text
  , taggerQLGenQCQuery :: TaggerQuery
  , taggerQLGenQueryInnerQuery :: Maybe TaggerQLGenQuery
  , taggerQLGenQueryCriteria :: QueryCriteria
  }
  deriving (Show, Eq)

generateAndRunQuery :: TermTree Text -> TaggedConnection -> IO (HashSet File)
generateAndRunQuery tt tc = undefined

runGenQuery :: TaggerQLGenQuery -> TaggedConnection -> IO (HashSet File)
runGenQuery q tc = undefined

generateQuerySeries :: TermTree Text -> [TaggerQLGenQuery]
generateQuerySeries tt =
  case tt of
    Simple (SimpleTerm t) ->
      [generateTermQuery t]
    Complex ct -> generateDepthComplexTermQuery ct

-- need to double check that this will produce terms in the correct order.
generateDepthComplexTermQuery :: ComplexTerm Text -> [TaggerQLGenQuery]
generateDepthComplexTermQuery (Bottom t) = [generateTermQuery t]
generateDepthComplexTermQuery (t :<- (ct :| cts)) =
  let thisTerm = (generateTermQuery t)
      thisTermSeries = setInnerQuery thisTerm <$> generateDepthComplexTermQuery ct
      thisWidthTermSeries =
        setInnerQuery thisTerm
          <$> (concat (generateDepthComplexTermQuery <$> cts))
   in thisTermSeries ++ thisWidthTermSeries

setInnerQuery :: TaggerQLGenQuery -> TaggerQLGenQuery -> TaggerQLGenQuery
setInnerQuery tq ic = tq{taggerQLGenQueryInnerQuery = Just ic}

generateTermQuery :: Term Text -> TaggerQLGenQuery
generateTermQuery (Term qc t) =
  TaggerQLGenQuery
    (if qc == UntaggedCriteria then Nothing else Just t)
    (generateQCCTEBody qc)
    Nothing
    qc

{- |
 Finally construct a query and return a tuple with its parameters.
-}
buildQuery :: TaggerQLGenQuery -> (TaggerQuery, [Text])
buildQuery (TaggerQLGenQuery p qcq Nothing qc) =
  (,maybeToList p)
    ( "WITH" `qcat` qcq
        `qcat` [r|SELECT t.* 
FROM Tag t 
JOIN qc USING|]
        `qcat` ("(" <> getJoinTypeId qc <> ")")
    )
buildQuery (TaggerQLGenQuery p qcq (Just iq) qc) =
  let (innerQuery, innerParams) = buildQuery iq
   in (,maybe innerParams (: innerParams) p)
        ( "WITH n AS (" `qcat` innerQuery `qcat` ")"
            `qcat` qcq
            `qcat` [r|SELECT t.* 
FROM Tag t 
JOIN n 
  ON t.subTagOfId = n.id 
JOIN qc USING|]
            `qcat` ("(" <> getJoinTypeId qc <> ")")
        )

{- |
 Creates a binding site for 1 parameter, unless the QC is UntaggedCriteria, then 0.

 Does not include the initial \"WITH\" or ", ". that must be constructed at runtime.
-}
generateQCCTEBody :: QueryCriteria -> TaggerQuery
generateQCCTEBody qc =
  "qc ("
    `qcat` getJoinTypeId qc
    `qcat` ") AS ("
    `qcat` ( case qc of
              DescriptorCriteria ->
                [r|SELECT id
FROM Descriptor
WHERE descriptor LIKE ? ESCAPE '\')|]
              MetaDescriptorCriteria ->
                [r|SELECT id
FROM Descriptor
WHERE descriptor LIKE ? ESCAPE '\'
UNION
SELECT infraDescriptorId
FROM MetaDescriptor md
JOIN qc
  ON md.infraDescriptorId = qc.descriptorId)|]
              FilePatternCriteria ->
                [r|SELECT id
FROM File
WHERE filePath LIKE ? ESCAPE '\')|]
              UntaggedCriteria ->
                [r|SELECT f.id
FROM File f
LEFT JOIN Tag t
  ON f.id = t.fileId
WHERE t.fileId IS NULL)|]
           )

getJoinTypeId :: QueryCriteria -> TaggerQuery
getJoinTypeId DescriptorCriteria = "descriptorId"
getJoinTypeId MetaDescriptorCriteria = "descriptorId"
getJoinTypeId _ = "fileId"
