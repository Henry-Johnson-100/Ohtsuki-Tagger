{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
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

  -- * Primitive Functions
  runExpr,
  evalExpr,

  -- ** For Testing
  runSubExprOnFile,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.HashSet (HashSet)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  RecordKey,
  insertTags,
  queryForDescriptorByPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
 )
import Database.Tagger.Query (
  queryForTagBySubTagTriple,
 )
import Database.Tagger.Type (
  Descriptor (descriptorId),
  File,
  Tag (tagId),
  TaggedConnection,
 )
import Text.Parsec.Error (errorMessages, messageString)
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Interpreter
import Text.TaggerQL.Expression.Parser (parseExpr, parseTagExpr)

{- |
 Run a TaggerQL query on the given database.
-}
runQuery :: TaggedConnection -> Text -> ExceptT [Text] IO (HashSet File)
runQuery c t =
  let result = parseExpr t
   in case result of
        Left pe -> throwE . map (T.pack . messageString) . errorMessages $ pe
        Right ex -> liftIO . flip runExpr c $ ex

{- |
 Query an 'Expression`
-}
runExpr :: Expression Identity Identity -> TaggedConnection -> IO (HashSet File)
runExpr expr = runReaderT (evalExpr expr)

evalExpr :: Expression Identity Identity -> ReaderT TaggedConnection IO (HashSet File)
evalExpr = flip runReaderT mempty . runInterpreter queryer

-- Tagging Engine

{- |
 Run a sub-expression, a subset of the TaggerQL, to tag a file with Descriptors
 matching the given patterns.

 Returns Just error messages if parsing fails. Otherwise Nothing.
-}
tagFile :: RecordKey File -> TaggedConnection -> Text -> IO (Maybe Text)
tagFile fk c =
  either
    (return . Just . T.pack . show)
    (\se -> runSubExprOnFile se fk c $> Nothing)
    . parseTagExpr

runSubExprOnFile :: SubExpression Identity -> RecordKey File -> TaggedConnection -> IO ()
runSubExprOnFile se fk c = void (runReaderT (insertSubExpr se Nothing) (fk, c))

insertSubExpr ::
  SubExpression Identity ->
  Maybe [RecordKey Tag] ->
  ReaderT (RecordKey File, TaggedConnection) IO [RecordKey Tag]
insertSubExpr se supertags =
  asks snd >>= \c ->
    ( case se of
        SubExpression (Identity (SubExpressionExtension tt se')) -> do
          insertedSubtags <- insertSubExpr (SubTag . Identity $ tt) supertags
          insertSubExpr se' (Just insertedSubtags)
        SubBinary (Identity (BinaryExpression se' _ se2)) -> do
          void $ insertSubExpr se' supertags
          void $ insertSubExpr se2 supertags
          -- tags inserted by a SubBinary is indeterminate and empty by default
          return mempty
        SubTag (Identity tt) -> do
          let txt = termTxt tt
          withDescriptors <- qDescriptor txt
          fk <- asks fst
          let tagTriples =
                (fk,,)
                  <$> (descriptorId <$> withDescriptors)
                    <*> maybe [Nothing] (fmap Just) supertags
          void . liftIO . insertTags tagTriples $ c
          -- Tag insertion may fail because some tags of the same form already exist.
          -- This query gets all of those pre-existing tags,
          -- and returns them as if they were just made.
          case supertags of
            -- If nothing, then these are top-level tags
            Nothing ->
              map tagId
                <$> liftIO
                  ( queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf
                      fk
                      txt
                      c
                  )
            Just _ ->
              -- If just, these are subtags of existing tags.
              map tagId . unions
                <$> liftIO
                  ( mapM
                      (`queryForTagBySubTagTriple` c)
                      (third fromJust <$> tagTriples)
                  )
    )
 where
  termTxt tt =
    case tt of
      DescriptorTerm txt -> txt
      MetaDescriptorTerm txt -> txt
  qDescriptor txt = asks snd >>= liftIO . queryForDescriptorByPattern txt
  unions :: (Foldable t, Eq a) => t [a] -> [a]
  unions xs = if null xs then [] else L.foldl' L.union [] xs
  third f (x, y, z) = (x, y, f z)
