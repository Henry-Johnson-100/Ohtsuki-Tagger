{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Tagger.Connection (
  -- * Wrapped types
  open,
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

import Control.Monad
import qualified Data.Text.IO as T.IO
import Data.Time
import qualified Database.SQLite.Simple as Simple
import qualified Database.SQLite3 as SQLite3
import Database.Tagger.Script
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

updateTaggerDBInfo :: BareConnection -> IO ()
updateTaggerDBInfo bc = do
  currentTime <- getCurrentTime
  dbInfoTableExists <- taggerDBInfoTableExists bc
  when dbInfoTableExists $ do
    bareExecute
      bc
      "UPDATE TaggerDBInfo SET version = ?, lastAccessed = ?"
      (taggerVersion, currentTime)

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
