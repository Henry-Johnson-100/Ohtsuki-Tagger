{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Tagger.Connection (
  open,
  query,
  query_,
  execute,
  execute_,
  Simple.Query,
  Simple.ToRow,
  Simple.FromRow,
) where

import Control.Monad
import qualified Data.Text.IO as T.IO
import Data.Time
import qualified Database.SQLite.Simple as Simple
import Database.Tagger.Type
import System.IO
import Tagger.Info

{- |
 Open a new 'TaggedConnection` with the database at the given path.

 The connection's label is set to the path and the lastAccessed time is
 updated.

 All Tagger connections should be made with this function.
-}
open :: FilePath -> IO TaggedConnection
open p = do
  currentTime <- getCurrentTime
  undefined

{- |
 Run a query with a 'TaggedConnection`

 If the tagged connection's connection is 'Nothing` then an error is printed and
 an empty result list is returned.
-}
query ::
  (Simple.ToRow q, Simple.FromRow r) =>
  TaggedConnection ->
  Simple.Query ->
  q ->
  IO [r]
query (TaggedConnection l mc) queryStmnt params =
  maybe
    (T.IO.hPutStrLn stderr ("Not connected to: " <> l) >> mempty)
    (\bc -> bareQuery bc queryStmnt params)
    mc

{- |
 Run a query taking no parameters with a 'TaggedConnection`

 If the tagged connection's connection is 'Nothing` then an error is printed and
 an empty result list is returned.
-}
query_ ::
  Simple.FromRow r =>
  TaggedConnection ->
  Simple.Query ->
  IO [r]
query_ (TaggedConnection l mc) queryStmnt =
  maybe
    (T.IO.hPutStrLn stderr ("Not connected to: " <> l) >> mempty)
    (`bareQuery_` queryStmnt)
    mc

{- |
 Execute a statement on a 'TaggedConnection`

 Executions return no results.

 If the tagged connection's connection is 'Nothing` then an error is printed.
-}
execute ::
  Simple.ToRow q =>
  TaggedConnection ->
  Simple.Query ->
  q ->
  IO ()
execute (TaggedConnection l mc) queryStmnt params =
  maybe
    (T.IO.hPutStrLn stderr ("Not connected to: " <> l) >> mempty)
    (\bc -> bareExecute bc queryStmnt params)
    mc

{- |
 Execute a statement taking no parameters on a 'TaggedConnection`

 Executions return no results.

 If the tagged connection's connection is 'Nothing` then an error is printed.
-}
execute_ ::
  TaggedConnection ->
  Simple.Query ->
  IO ()
execute_ (TaggedConnection l mc) queryStmnt =
  maybe
    (T.IO.hPutStrLn stderr ("Not connected to: " <> l) >> mempty)
    (`bareExecute_` queryStmnt)
    mc

updateLastAccessed :: BareConnection -> IO ()
updateLastAccessed = undefined

getLastAccessed :: BareConnection -> IO ()
getLastAccessed = undefined

updateLastBackupDateTime :: BareConnection -> IO ()
updateLastBackupDateTime = undefined

getLastBackupDateTime :: BareConnection -> IO ()
getLastBackupDateTime = undefined

activateForeignKeyPragma :: BareConnection -> IO ()
activateForeignKeyPragma = flip bareExecute_ "PRAGMA foreign_keys = on"

-- taggerDBInfoTableExists :: BareConnection -> IO Bool
-- taggerDBInfoTableExists c = do
--   r <-
--     query_
--       (_bareConnection c)
--       "SELECT COUNT(*) \
--       \FROM sqlite_master \
--       \WHERE type = 'table' AND name = 'TaggerDBInfo'" ::
--       IO [Only Int]
--   return . all ((> 0) . (\(Only n) -> n)) $ r

-- updateTaggerDBInfo :: BareConnection -> IO ()
-- updateTaggerDBInfo bc = do
--   let c = _bareConnection bc
--   currentTime <- getCurrentTime
--   dbInfoTableExists <- taggerDBInfoTableExists c
--   when dbInfoTableExists $ do
--     _ <- getLastBackupDateTime bc
--     execute
--       c
--       "UPDATE TaggerDBInfo SET version = ?, lastAccessed = ?"
--       (taggerVersion, currentTime)

bareQuery ::
  (Simple.ToRow q, Simple.FromRow r) =>
  BareConnection ->
  Simple.Query ->
  q ->
  IO [r]
bareQuery = bareConnectionAction Simple.query

bareQuery_ :: Simple.FromRow r => BareConnection -> Simple.Query -> IO [r]
bareQuery_ = bareConnectionAction Simple.query_

bareExecute :: Simple.ToRow q => BareConnection -> Simple.Query -> q -> IO ()
bareExecute = bareConnectionAction Simple.execute

bareExecute_ :: BareConnection -> Simple.Query -> IO ()
bareExecute_ = bareConnectionAction Simple.execute_

bareConnectionAction :: (Simple.Connection -> t) -> BareConnection -> t
bareConnectionAction f bc = f (_bareConnection bc)