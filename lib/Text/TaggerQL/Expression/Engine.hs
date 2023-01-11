{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Move guards forward" #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune #-}

{-# HLINT ignore "Use lambda-case" #-}

{- |
Module      : Text.TaggerQL.Expression.Engine
Description : The interpreter for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Contains functions that interprets the TaggerQL query language to either run queries
  or tag a file with a certain expression.
-}
module Text.TaggerQL.Expression.Engine (
  runQuery,
  tagFile,

  -- * Interpreters
  ExpressionInterpreter (..),
  runExpressionInterpreter,
  SubExpressionInterpreter (..),
  runSubExpressionInterpreter,

  -- ** Examples
  identityInterpreter,

  -- * Indexing
  ExpressionIndex (..),
  exprAt,
  subExprAtL,
  foldIxGen,
  flatten,
  index,
  indexWith,

  -- * Modifying
  liftSubExpression,
  lowerExpression,

  -- * Primitive Functions
  runExpr,
  interpretQuery,

  -- ** For Testing
  applyTExpressionToFile,
  evalSubExpression,
  queryTagSet',
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, void, when, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.State.Strict (StateT, evalState, get, gets, modify, runState)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bitraversable (bitraverse)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Connection (query)
import Database.Tagger.Query (
  allFiles,
  allTags,
  insertTags,
  queryForDescriptorByPattern,
  queryForFileByPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,
 )
import Database.Tagger.Query.Type (TaggerQuery)
import Database.Tagger.Type (
  Descriptor (descriptorId),
  File,
  RecordKey,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
 )
import Lens.Micro (Lens', lens, (%~), (&), (.~), (^.))
import Text.Parsec.Error (errorMessages, messageString)
import Text.RawString.QQ (r)
import Text.TaggerQL.Expression.AST (
  BinaryOperation (..),
  DTerm (..),
  Endomorphism (..),
  Expression (..),
  FExpression,
  FileTerm,
  Lng (..),
  Pattern (..),
  SubExpression (..),
  TExpression (TValue),
  TagTerm (..),
  TagTermExtension (..),
  distribute,
  evalDistribute,
  evaluateTExpression,
  evaluateUExpression,
  extensionL,
  liftFExpressionA,
  runDTerm,
 )
import Text.TaggerQL.Expression.Parser (parseFExpr, parseTExpr)
import Prelude hiding (lookup, (!!))

{- |
 Defines a computation for each constructor of a 'SubExpression`.

 Used to monadically fold a 'SubExpression`.
-}
data SubExpressionInterpreter n b = SubExpressionInterpreter
  { interpretSubTag :: TagTerm -> n b
  , interpretBinarySubExpression :: BinaryOperation b -> n b
  , interpretSubExpression :: TagTermExtension (n b) -> n b
  }

{- |
 Run the given 'SubExpressionInterpreter` over a 'SubExpression`.
-}
runSubExpressionInterpreter ::
  Monad n =>
  SubExpressionInterpreter n b ->
  SubExpression ->
  n b
runSubExpressionInterpreter setr@(SubExpressionInterpreter a b c) se = case se of
  SubTag tt -> a tt
  BinarySubExpression (BinaryOperation lhs so rhs) -> do
    lhsR <- runSubExpressionInterpreter setr lhs
    rhsR <- runSubExpressionInterpreter setr rhs
    b $ BinaryOperation lhsR so rhsR
  SubExpression (TagTermExtension tt se') ->
    c
      . TagTermExtension tt
      . runSubExpressionInterpreter setr
      $ se'

{- |
 Defines a computation for each constructor of an 'Expression`.

 Used to monadically fold an 'Expression`.
-}
data ExpressionInterpreter m b a = ExpressionInterpreter
  { interpretFileTerm :: FileTerm -> m a
  , interpretTagTerm :: TagTerm -> m a
  , interpretTagExpression :: TagTermExtension (m b) -> m a
  , interpretBinaryExpression :: BinaryOperation a -> m a
  , subExpressionInterpreter :: SubExpressionInterpreter m b
  }

{- |
 A baseline 'Interpreter` to build others with.
-}
identityInterpreter :: Monad m => ExpressionInterpreter m SubExpression Expression
identityInterpreter =
  ExpressionInterpreter
    { subExpressionInterpreter =
        SubExpressionInterpreter
          { interpretSubTag = return . SubTag
          , interpretBinarySubExpression = return . BinarySubExpression
          , interpretSubExpression = \(TagTermExtension tt se) ->
              se >>= return . SubExpression . TagTermExtension tt
          }
    , interpretFileTerm = return . FileTermValue
    , interpretTagTerm = return . TagTermValue
    , interpretBinaryExpression = return . BinaryExpression
    , interpretTagExpression = \(TagTermExtension tt se) ->
        se >>= return . TagExpression . TagTermExtension tt
    }

{- |
 Run the given 'ExpressionInterpreter` over an 'Expression`. Appropriately
 sequencing monadic actions and computational side effects when extending the
 'Expression` with a 'SubExpression`.
-}
runExpressionInterpreter :: Monad m => ExpressionInterpreter m b a -> Expression -> m a
runExpressionInterpreter itr@(ExpressionInterpreter ift itt itte ibo setr) expr =
  case expr of
    FileTermValue ft -> ift ft
    TagTermValue tt -> itt tt
    TagExpression (TagTermExtension tt se) ->
      itte
        . TagTermExtension tt
        . runSubExpressionInterpreter setr
        $ se
    BinaryExpression (BinaryOperation lhs so rhs) -> do
      lhsR <- runExpressionInterpreter itr lhs
      rhsR <- runExpressionInterpreter itr rhs
      ibo $ BinaryOperation lhsR so rhsR

joinSubtags :: HashSet Tag -> HashSet (Maybe (RecordKey Tag)) -> HashSet Tag
joinSubtags supertags subtags =
  HS.filter (\(Just . tagId -> supertagId) -> HS.member supertagId subtags) supertags

{- |
 Run a TaggerQL query on the given database.
-}
runQuery :: TaggedConnection -> Text -> ExceptT [Text] IO (HashSet File)
runQuery c t =
  let result = parseFExpr t
   in case result of
        Left pe -> throwE . map (T.pack . messageString) . errorMessages $ pe
        Right ex -> liftIO . flip runExpr c $ ex

{- |
 Query an 'Expression`
-}
runExpr :: FExpression (DTerm Pattern) Pattern -> TaggedConnection -> IO (HashSet File)
runExpr expr = runReaderT (interpretQuery expr)

{- |
 To the given set of tags, join all tags computed by the given TExpression by those that
 are subtags.
-}
evalSubExpression ::
  TExpression (DTerm Pattern) ->
  -- | The current 'Tag` environment. Any set of 'Tag` computed by this function
  -- will be a subset of this argument.
  HashSet Tag ->
  ReaderT
    TaggedConnection
    IO
    (HashSet Tag)
evalSubExpression subExpr supertags =
  runTagSet
    . evalDistribute
    . evaluateTExpression
    . fmap distribute
    . ((TValue . TagSet $ supertags) @>)
    <$> traverse queryTagSet subExpr

toFileSet :: HashSet Tag -> TaggedConnection -> IO (HashSet File)
toFileSet (map tagId . HS.toList -> ts) conn = do
  results <- mapM (query conn q . (: [])) ts
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

tagQueryOnDescriptorPattern :: TaggerQuery
tagQueryOnDescriptorPattern =
  [r|
    SELECT
      t.id,
      t.fileId,
      t.descriptorId,
      t.subTagOfId
    FROM Tag t
    JOIN Descriptor d ON t.descriptorId = d.id
    WHERE d.descriptor LIKE ? ESCAPE '\'
    |]

tagQueryOnMetaDescriptorPattern :: TaggerQuery
tagQueryOnMetaDescriptorPattern =
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

-- Tagging TExpression Engine

{- |
 Run a sub-expression, a subset of the TaggerQL, to tag a file with Descriptors
 matching the given patterns.

 Returns Just error messages if parsing fails. Otherwise Nothing.
-}
tagFile :: RecordKey File -> TaggedConnection -> Text -> IO (Maybe Text)
tagFile fk c t =
  let parseResult = parseTExpr t
   in case parseResult of
        Left pe ->
          return
            . Just
            . T.unwords
            . map (T.pack . messageString)
            . errorMessages
            $ pe
        Right te -> Nothing <$ applyTExpressionToFile (fmap runDTerm te) fk c

newtype TagApplication = TagApplication
  { runTagApplication ::
      Maybe [RecordKey Tag] ->
      RecordKey File ->
      TaggedConnection ->
      IO [RecordKey Tag]
  }

instance Endomorphism TagApplication where
  (@>) :: TagApplication -> TagApplication -> TagApplication
  (TagApplication f) @> (TagApplication g) = TagApplication $ \mts fk c -> do
    higherEnvResults <- f mts fk c
    g (Just higherEnvResults) fk c

instance Lng TagApplication where
  (<+>) :: TagApplication -> TagApplication -> TagApplication
  (<+>) = disregardBinaryOp
  (<^>) :: TagApplication -> TagApplication -> TagApplication
  (<^>) = disregardBinaryOp
  (<->) :: TagApplication -> TagApplication -> TagApplication
  (<->) = disregardBinaryOp

disregardBinaryOp :: TagApplication -> TagApplication -> TagApplication
disregardBinaryOp (TagApplication f) (TagApplication g) =
  TagApplication $ \mts fk c -> do
    _ <- f mts fk c
    _ <- g mts fk c
    return []

insertPattern :: Pattern -> TagApplication
insertPattern WildCard = TagApplication $ \_ _ _ -> return []
insertPattern (PatternText t) = TagApplication $ \mts fk c -> do
  descriptors <- queryForDescriptorByPattern t c
  let taggingTriples =
        (fk,,) <$> map descriptorId descriptors
          <*> maybe [Nothing] (map Just) mts
  void . insertTags taggingTriples $ c
  -- Tag insertion may fail because some tags of the same form already exist.
  -- This query gets all of those pre-existing tags,
  -- and returns them as if they were just made.
  case mts of
    -- If nothing, then these are top-level tags
    Nothing ->
      map tagId <$> queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf fk t c
    Just _ ->
      -- If just, these are subtags of existing tags.
      map tagId . unions
        <$> mapM
          (`queryForTagBySubTagTriple` c)
          (third fromJust <$> taggingTriples)
 where
  unions xs = if null xs then [] else L.foldl' L.union [] xs
  third f (x, y, z) = (x, y, f z)

applyTExpressionToFile ::
  TExpression Pattern ->
  RecordKey File ->
  TaggedConnection ->
  IO [RecordKey Tag]
applyTExpressionToFile texpr =
  runTagApplication
    (evalDistribute . evaluateTExpression . fmap (distribute . insertPattern) $ texpr)
    Nothing

{- |
 Where 'e` is 1-indexed in order of evaluation.
-}
class ExpressionIndex e where
  -- | Find a subset of 'e` at the given index if it exists.
  lookup :: Int -> e -> Maybe e

  -- | Replace a location in the second set with first, if that location exists.
  replace :: Int -> e -> e -> e

  -- | An infix variant of 'lookup`
  (!!) :: e -> Int -> Maybe e
  e !! n = lookup n e

instance ExpressionIndex Expression where
  lookup :: Int -> Expression -> Maybe Expression
  lookup n = snd . snd . flip runState (1, Nothing) . runExpressionInterpreter itr
   where
    itr =
      ExpressionInterpreter
        { subExpressionInterpreter = subExpressionInterpreter identityInterpreter
        , interpretFileTerm = stlookupWith n . FileTermValue
        , interpretTagTerm = stlookupWith n . TagTermValue
        , interpretBinaryExpression = stlookupWith n . BinaryExpression
        , interpretTagExpression = \(TagTermExtension tt se) ->
            se >>= stlookupWith n . TagExpression . TagTermExtension tt
        }
  replace :: Int -> Expression -> Expression -> Expression
  replace n replaceWith = flip evalState 1 . runExpressionInterpreter itr
   where
    itr =
      ExpressionInterpreter
        { subExpressionInterpreter = subExpressionInterpreter identityInterpreter
        , interpretFileTerm = stReplaceWith n replaceWith . FileTermValue
        , interpretTagTerm = stReplaceWith n replaceWith . TagTermValue
        , interpretBinaryExpression = stReplaceWith n replaceWith . BinaryExpression
        , interpretTagExpression = \(TagTermExtension tt se) ->
            se >>= stReplaceWith n replaceWith . TagExpression . TagTermExtension tt
        }

instance ExpressionIndex SubExpression where
  lookup :: Int -> SubExpression -> Maybe SubExpression
  lookup n = snd . snd . flip runState (1, Nothing) . runSubExpressionInterpreter itr
   where
    itr =
      SubExpressionInterpreter
        { interpretSubTag = stlookupWith n . SubTag
        , interpretBinarySubExpression = stlookupWith n . BinarySubExpression
        , interpretSubExpression = \(TagTermExtension tt se) ->
            se >>= stlookupWith n . SubExpression . TagTermExtension tt
        }
  replace :: Int -> SubExpression -> SubExpression -> SubExpression
  replace n replaceWith = flip evalState 1 . runSubExpressionInterpreter itr
   where
    itr =
      SubExpressionInterpreter
        { interpretSubTag = stReplaceWith n replaceWith . SubTag
        , interpretBinarySubExpression = stReplaceWith n replaceWith . BinarySubExpression
        , interpretSubExpression = \(TagTermExtension tt se) ->
            se >>= stReplaceWith n replaceWith . SubExpression . TagTermExtension tt
        }

{- |
 Lens for indexing an 'ExpressionIndex`
-}
exprAt :: ExpressionIndex e => Int -> Lens' e (Maybe e)
exprAt n =
  lens
    (lookup n)
    (\expr mReplace -> maybe expr (\re -> replace n re expr) mReplace)

{- |
 A less general lens for traversing an 'Expression` for a 'SubExpression`

 Where the first integer is an index of an expression and the second is the index
 of a subexpression contained in it.
-}
subExprAtL :: Int -> Int -> Lens' Expression (Maybe SubExpression)
subExprAtL exprIx seIx =
  lens
    ( \expr -> case expr ^. exprAt exprIx of
        Just ex ->
          case ex of
            TagExpression tte -> tte ^. extensionL . exprAt seIx
            _noSE -> Nothing
        _noEx -> Nothing
    )
    ( \expr mse ->
        expr & exprAt exprIx
          %~ fmap
            ( \ex -> case ex of
                TagExpression tte ->
                  TagExpression $ tte & extensionL %~ exprAt seIx .~ mse
                _noSE -> ex
            )
    )

{- |
 Helper function for defining instances of 'ExpressionIndex.lookup`
-}
stlookupWith ::
  (Monad m, Num a, Eq a) =>
  a ->
  b ->
  StateT (a, Maybe b) m b
stlookupWith n x = do
  st <- gets fst
  modify . first $ (1 +)
  when (st == n) . modify . second . const . Just $ x
  return x

{- |
 Helper function for defining instance of 'ExpressionIndex.replace`
-}
stReplaceWith :: (Monad m, Num a, Eq a) => a -> b -> b -> StateT a m b
stReplaceWith n t e = do
  st <- get
  modify (1 +)
  if st == n
    then return t
    else return e

{- |
 Flatten an 'ExpressionIndex` into its respective components along with their indices
 in reverse evaluation order.

 Such that:

 @e = (snd . head . flatten) e@
-}
flatten :: ExpressionIndex e => e -> [(Int, e)]
flatten = foldIxGen (flip (:)) []

{- |
 Generate an infinite indexed list in evaluation order from an expression.

 Calling @takeWhile (isJust . snd)@
  will return a finite list of the expression's contents.
-}
exprIxGen :: ExpressionIndex e => e -> [(Int, Maybe e)]
exprIxGen e = [(n, e !! n) | n <- [1 ..]]

{- |
 foldl' over a flattened 'ExpressionIndex` in reverse evaluation order.
-}
foldIxGen :: ExpressionIndex e => (a -> (Int, e) -> a) -> a -> e -> a
foldIxGen foldF accum = go foldF accum . exprIxGen
 where
  go _ acc [] = acc
  go _ acc ((_, Nothing) : _) = acc
  go f acc (x : xs) = go f (f acc . second fromJust $ x) xs

{- |
 Find the latest evaluating subset of the expression.
-}
index :: (ExpressionIndex a, Eq a) => a -> a -> Maybe (Int, a)
index needle = indexWith (== needle)

{- |
 Find the latest evaluating expression that satisfies the predicate.
-}
indexWith :: ExpressionIndex t => (t -> Bool) -> t -> Maybe (Int, t)
indexWith p = foldIxGen (\m x@(_, a) -> m <|> (x <$ guard (p a))) Nothing

{- |
 Lifts a 'SubExpression` out of the domain of 'Tag` and in to the domain of 'File`
-}
liftSubExpression :: SubExpression -> Expression
liftSubExpression se = case se of
  SubTag tt -> TagTermValue tt
  BinarySubExpression (BinaryOperation lhs so rhs) ->
    BinaryExpression (BinaryOperation (liftSubExpression lhs) so (liftSubExpression rhs))
  SubExpression tte -> TagExpression tte

{- |
 Attempt to lower an 'Expression` from the domain of 'File` to the domain of 'Tag`.

 Fails if the expression contains a 'FileTermValue`.
-}
lowerExpression :: Expression -> Maybe SubExpression
lowerExpression expr = case expr of
  FileTermValue _ -> Nothing
  TagTermValue tt -> Just . SubTag $ tt
  TagExpression tte -> Just . SubExpression $ tte
  BinaryExpression (BinaryOperation lhs so rhs) ->
    BinarySubExpression
      <$> (BinaryOperation <$> lowerExpression lhs <*> pure so <*> lowerExpression rhs)

-- FExpression interpreter

{- |
 Wrapper to define an 'Lng` instance.
-}
newtype FileSet = FileSet {runFileSet :: HashSet File} deriving (Show)

{- |
 Lng over the Ring of 'HashSet File`
-}
instance Lng FileSet where
  (<+>) :: FileSet -> FileSet -> FileSet
  (FileSet x) <+> (FileSet y) = FileSet $ HS.union x y
  (<^>) :: FileSet -> FileSet -> FileSet
  (FileSet x) <^> (FileSet y) = FileSet $ HS.intersection x y
  (<->) :: FileSet -> FileSet -> FileSet
  (FileSet x) <-> (FileSet y) = FileSet $ HS.difference x y

{- |
 Wrapper to define an 'Lng` and 'Endomorphism` instance.
-}
newtype TagSet = TagSet {runTagSet :: HashSet Tag} deriving (Show)

{- |
 Lng over the Ring of 'HashSet Tag`
-}
instance Lng TagSet where
  (<+>) :: TagSet -> TagSet -> TagSet
  (<+>) = tagSetWithSetOp HS.union
  (<^>) :: TagSet -> TagSet -> TagSet
  (<^>) = tagSetWithSetOp HS.intersection
  (<->) :: TagSet -> TagSet -> TagSet
  (<->) = tagSetWithSetOp HS.difference

tagSetWithSetOp ::
  (HashSet Tag -> HashSet Tag -> HashSet Tag) ->
  TagSet ->
  TagSet ->
  TagSet
tagSetWithSetOp so (TagSet x) (TagSet y) = TagSet $ x `so` y

{- |
 Filters the left-hand set of tags if there is a subtag in the right-hand.
-}
instance Endomorphism TagSet where
  (@>) :: TagSet -> TagSet -> TagSet
  (TagSet x) @> (TagSet y) = TagSet $ joinSubtags x (HS.map tagSubtagOfId y)

interpretQuery ::
  FExpression (DTerm Pattern) Pattern ->
  ReaderT TaggedConnection IO (HashSet File)
interpretQuery =
  fmap (runFileSet . evaluateUExpression)
    . liftFExpressionA resolveTagSet
    <=< bitraverse queryTagSet queryFilePattern

queryFilePattern :: Pattern -> ReaderT TaggedConnection IO FileSet
queryFilePattern p =
  ask
    >>= case p of
      WildCard -> fmap (FileSet . HS.fromList) . liftIO . allFiles
      PatternText t ->
        fmap (FileSet . HS.fromList) . liftIO . queryForFileByPattern t

resolveTagSet :: MonadIO m => TExpression TagSet -> ReaderT TaggedConnection m FileSet
resolveTagSet texpr = do
  c <- ask
  liftIO
    . fmap FileSet
    . flip toFileSet c
    . runTagSet
    . evalDistribute
    . evaluateTExpression
    . fmap distribute
    $ texpr

queryTagSet :: DTerm Pattern -> ReaderT TaggedConnection IO TagSet
queryTagSet = fmap TagSet . queryTagSet'

queryTagSet' :: DTerm Pattern -> ReaderT TaggedConnection IO (HashSet Tag)
queryTagSet' dt = case dt of
  DTerm p -> case p of
    WildCard -> ask >>= liftIO . fmap HS.fromList . allTags
    PatternText t -> do
      c <- ask
      liftIO . fmap HS.fromList $ query c tagQueryOnDescriptorPattern [t]
  DMetaTerm p -> case p of
    WildCard -> ask >>= liftIO . fmap HS.fromList . allTags
    PatternText t -> do
      c <- ask
      liftIO . fmap HS.fromList $ query c tagQueryOnMetaDescriptorPattern [t]
