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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.HashSet (HashSet)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  RecordKey,
 )
import Database.Tagger.Type (
  File,
  TaggedConnection,
 )
import Text.Parsec.Error (errorMessages, messageString)
import Text.TaggerQL.Expression.AST (Expression, SubExpression)
import Text.TaggerQL.Expression.Interpreter (
  queryer,
  runInterpreter,
  runSubInterpreter,
  tagger,
 )
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
runSubExprOnFile se fk c =
  ()
    <$ runReaderT (runSubInterpreter tagger se) (fk, Nothing, c)
