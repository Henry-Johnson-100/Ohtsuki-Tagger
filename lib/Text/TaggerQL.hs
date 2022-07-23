{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use concatMap" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL
Description : The front-end and interface for running TaggerQL queries on a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL (
  -- * Query with TaggerQL
  TaggerQLQuery (..),
  taggerQL,
  queryRequest,

  -- * Tag with TaggerQL
  TaggerQLTagStmnt (..),
  taggerQLTag,

  -- * Other
  CombinableSentenceResult,
  combinableSentenceResultSetOp,
  combinableSentenceResultSet,
) where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  ask,
  asks,
  local,
 )
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Query (
  insertTags,
  queryForDescriptorByPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,
 )
import Database.Tagger.Type (
  Descriptor (descriptorId),
  File,
  RecordKey,
  Tag (tagId),
  TaggedConnection,
 )
import System.IO (hPrint, stderr)
import Text.TaggerQL.AST (
  ComplexTerm (..),
  Sentence (Sentence),
  SimpleTerm (..),
  Term (Term),
  TermTree (..),
  complexTermNode,
 )
import Text.TaggerQL.Engine.QueryEngine (
  CombinableSentenceResult,
  TaggerQLQuery (..),
  combinableSentenceResultSet,
  combinableSentenceResultSetOp,
  queryRequest,
  taggerQL,
 )
import Text.TaggerQL.Parser.Internal (
  parse,
  sentenceParser,
 )

{- |
 newtype wrapper for 'Text`

 is an instance of IsString
-}
newtype TaggerQLTagStmnt = TaggerQLTagStmnt Text deriving (Show, Eq)

instance IsString TaggerQLTagStmnt where
  fromString = TaggerQLTagStmnt . T.pack

newtype TermTag = TermTag (Term Text) deriving (Show, Eq)

newtype TermSubTag = TermSubTag (Term Text) deriving (Show, Eq)

type TaggingEnvironment = (TaggedConnection, [RecordKey Tag])

type TaggingReader a = ReaderT TaggingEnvironment IO a

setTagList :: [RecordKey Tag] -> TaggingEnvironment -> TaggingEnvironment
setTagList t (c, _) = (c, t)

{- |
 Run a subset of the TaggerQL as 'Descriptor` patterns to insert as tags on the given
 'File`.

 Because it is a subset of TaggerQL, 'QueryCriteria` Literals can be parsed, but they
 will be ignored during insertion. If it is desired to 'Tag` a 'File` with 'Descriptor`s
 that look like 'QueryCriteria` Literals, then use escape characters.
-}
taggerQLTag :: RecordKey File -> TaggerQLTagStmnt -> TaggedConnection -> IO ()
taggerQLTag fk (TaggerQLTagStmnt q) tc = do
  let tagStmnt = parse sentenceParser "TaggerQLStmnt" q
  either
    (hPrint stderr)
    (insertTagSentence tc . zipDescriptorPatternsWithFileKey fk)
    tagStmnt
 where
  zipDescriptorPatternsWithFileKey ::
    RecordKey File ->
    Sentence Text ->
    Sentence (RecordKey File, Text)
  zipDescriptorPatternsWithFileKey fk' = fmap (fk',)

insertTagSentence :: TaggedConnection -> Sentence (RecordKey File, Text) -> IO ()
insertTagSentence tc (Sentence tts) = mapM_ (insertTagTree tc) tts

insertTagTree :: TaggedConnection -> TermTree (RecordKey File, Text) -> IO ()
insertTagTree tc tt =
  case tt of
    Simple t -> void (runReaderT (insertSimpleTerm t) (tc, []))
    Complex t ->
      void
        (runReaderT (insertComplexTerm True t) (tc, []))

insertComplexTerm ::
  Bool -> ComplexTerm (RecordKey File, Text) -> TaggingReader [RecordKey Tag]
insertComplexTerm _ (Bottom _) = return []
insertComplexTerm True ct = do
  newEnvList <- insertTopLevelComplexTerm ct
  local (setTagList newEnvList) $ insertComplexTerm False ct
 where
  insertTopLevelComplexTerm ::
    ComplexTerm (RecordKey File, Text) ->
    TaggingReader [RecordKey Tag]
  insertTopLevelComplexTerm (complexTermNode -> t@(Term _ (fk, p))) = do
    void $ insertTerm t
    c <- asks fst
    map tagId
      <$> lift
        ( queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf
            fk
            p
            c
        )
insertComplexTerm _ (currentTerm :<- (ct :| sts)) = do
  void $ case ct of
    Bottom t -> insertSubTag t
    (complexTermNode -> ctt) -> do
      lowerLayerEnv <- insertSubTag ctt
      local (setTagList lowerLayerEnv) $ insertComplexTerm False ct
  mapM_ (\ct' -> insertComplexTerm False (currentTerm :<- (ct' :| []))) sts
  return []

insertSubTag ::
  Term (RecordKey File, Text) ->
  ReaderT TaggingEnvironment IO [RecordKey Tag]
insertSubTag (Term _ (fk, p)) = do
  te <- ask
  subTagDescriptorIds <-
    map descriptorId
      <$> lift (queryForDescriptorByPattern p (fst te))
  let subTagTriples = (fk,,) <$> subTagDescriptorIds <*> (Just <$> snd te)
  lift . void $ insertTags subTagTriples (fst te)
  map tagId . unions
    <$> lift
      ( mapM
          (`queryForTagBySubTagTriple` fst te)
          (third fromJust <$> subTagTriples)
      )
 where
  third f (x, y, z) = (x, y, f z)

insertSimpleTerm ::
  SimpleTerm (RecordKey File, Text) ->
  TaggingReader [RecordKey Tag]
insertSimpleTerm (SimpleTerm t) = insertTerm t

insertTerm :: Term (RecordKey File, Text) -> TaggingReader [RecordKey Tag]
insertTerm (Term _ (fk, p)) = do
  c <- asks fst
  ds <- map descriptorId <$> lift (queryForDescriptorByPattern p c)
  let tagTriples = (fk,,Nothing) <$> ds
  lift $ insertTags tagTriples c

unions :: (Foldable t, Eq a) => t [a] -> [a]
unions xs = if null xs then [] else L.foldl' L.union [] xs
