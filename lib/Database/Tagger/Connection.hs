{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

{- |
Module      : Database.Tagger.Connection
Description : Module to open, query, and close Tagger database connections.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger.Connection (
  -- * Wrapped types
  open,
  open',
  close,
  query,
  query_,
  execute,
  execute_,
  initializeDatabase,

  -- * Internal Connection types

  -- | Functions exposing internal Connection representations. Exposed for convenience.
  withBareConnection,
  withConnection,
  withConnectionHandle,

  -- * Database.SQLite.Simple types

  -- | Types exported from Database.SQLite.Simple for use in 'Database.Tagger`
  Simple.Query,
  Simple.ToRow,
  Simple.FromRow,
  Simple.Only (..),
) where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Time (getCurrentTime)
import qualified Database.SQLite.Simple as Simple
import qualified Database.SQLite3 as SQLite3
import Database.Tagger.Script (
  SQLiteScript (SQLiteScript),
  schemaDefinition,
 )
import Database.Tagger.Type.Prim (BareConnection (..), TaggedConnection (..))
import System.IO (stderr)
import Tagger.Info (taggerVersion)

{- |
 Open a new 'TaggedConnection` with the database at the given path.

 The connection's label is set to the path and the lastAccessed time and database version
 is updated.

 If the db info table is not found then the database is initialized, if this is undesired,
  use open'

 Most Tagger connections should be made with this function.
-}
open :: FilePath -> IO TaggedConnection
open p = do
  let tagName = T.pack p
  bc <- fmap BareConnection . Simple.open $ p
  dbInfoTableExists <- taggerDBInfoTableExists bc
  unless
    dbInfoTableExists
    ( withConnection
        (withConnectionHandle (`SQLite3.exec` (\(SQLiteScript s) -> s) schemaDefinition))
        bc
    )
  activateForeignKeyPragma bc
  updateTaggerDBInfoVersion bc
  updateTaggerDBInfoLastAccessed bc
  return $ TaggedConnection tagName (Just bc)

{- |
 Like 'open` but does NOT update the table with lastAccessedDateTime or the table version.

 Also does not attempt to initialize the database if there is no db info table.
-}
open' :: FilePath -> IO TaggedConnection
open' p = do
  let tagName = T.pack p
  bc <- fmap BareConnection . Simple.open $ p
  activateForeignKeyPragma bc
  return $ TaggedConnection tagName (Just bc)

{- |
 Given a 'TaggedConnection`, will close it and return a 'TaggedConnection` with the same
 label a 'Nothing` 'BareConnection`.

 Does nothing if the 'BareConnection` is already 'Nothing`.
-}
close :: TaggedConnection -> IO TaggedConnection
close tc@(TaggedConnection _ mbc) =
  maybe
    (return tc)
    ( \bc -> do
        withConnection Simple.close bc
        return (tc{_taggedconnectionConnInstance = Nothing})
    )
    mbc

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
query tc queryStmnt params = withBareConnection (\bc -> bareQuery bc queryStmnt params) tc

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
query_ tc queryStmnt = withBareConnection (`bareQuery_` queryStmnt) tc

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
execute tc queryStmnt params =
  withBareConnection
    (\bc -> bareExecute bc queryStmnt params)
    tc

{- |
 Execute a statement taking no parameters on a 'TaggedConnection`

 Executions return no results.

 If the tagged connection's connection is 'Nothing` then an error is printed.
-}
execute_ ::
  TaggedConnection ->
  Simple.Query ->
  IO ()
execute_ tc queryStmnt =
  withBareConnection
    (`bareExecute_` queryStmnt)
    tc

{- |
 Run the Tagger schema definition script on the given connection.

 Should ideally not do anything on a database that is already up-to-date with the current
 schema definition, but it would be best to avoid doing that anyways.
-}
initializeDatabase :: TaggedConnection -> IO ()
initializeDatabase =
  withBareConnection
    ( withConnection
        ( withConnectionHandle
            (`SQLite3.exec` (\(SQLiteScript s) -> s) schemaDefinition)
        )
    )

{- |
 Run a monoidal IO action using a 'TaggedConnection`'s 'BareConnection`.
-}
withBareConnection :: Monoid b => (BareConnection -> IO b) -> TaggedConnection -> IO b
withBareConnection f tc =
  maybe
    ( T.IO.hPutStrLn stderr ("Not Connected to " <> _taggedconnectionConnName tc)
        >> mempty
    )
    f
    . _taggedconnectionConnInstance
    $ tc

{- |
 Run an action using a 'BareConnection`'s Connection.
-}
withConnection :: (Simple.Connection -> t) -> BareConnection -> t
withConnection f = f . _bareConnection

{- |
 Run an action using a Connection's ConnectionHandle.
-}
withConnectionHandle :: (SQLite3.Database -> c) -> Simple.Connection -> c
withConnectionHandle f = f . Simple.connectionHandle

activateForeignKeyPragma :: BareConnection -> IO ()
activateForeignKeyPragma = flip bareExecute_ "PRAGMA foreign_keys = on"

taggerDBInfoTableExists :: BareConnection -> IO Bool
taggerDBInfoTableExists c = do
  r <-
    bareQuery_
      c
      "SELECT COUNT(*) \
      \FROM sqlite_master \
      \WHERE type = 'table' AND name = 'TaggerDBInfo'" ::
      IO [Simple.Only Int]
  return . all ((> 0) . (\(Simple.Only n) -> n)) $ r

updateTaggerDBInfoLastAccessed :: BareConnection -> IO ()
updateTaggerDBInfoLastAccessed bc = do
  dbInfoTableExists <- taggerDBInfoTableExists bc
  when dbInfoTableExists $ do
    currentTime <- getCurrentTime
    withConnection
      ( \c ->
          Simple.execute
            c
            "UPDATE TaggerDBInfo SET lastAccessed = ?"
            [currentTime]
      )
      bc

updateTaggerDBInfoVersion :: BareConnection -> IO ()
updateTaggerDBInfoVersion bc = do
  dbInfoTableExists <- taggerDBInfoTableExists bc
  when
    dbInfoTableExists
    ( withConnection
        ( \c ->
            Simple.execute c "UPDATE TaggerDBInfo SET version = ?" [taggerVersion]
        )
        bc
    )

bareQuery ::
  (Simple.ToRow q, Simple.FromRow r) =>
  BareConnection ->
  Simple.Query ->
  q ->
  IO [r]
bareQuery = withConnection Simple.query

bareQuery_ :: Simple.FromRow r => BareConnection -> Simple.Query -> IO [r]
bareQuery_ = withConnection Simple.query_

bareExecute :: Simple.ToRow q => BareConnection -> Simple.Query -> q -> IO ()
bareExecute = withConnection Simple.execute

bareExecute_ :: BareConnection -> Simple.Query -> IO ()
bareExecute_ = withConnection Simple.execute_
