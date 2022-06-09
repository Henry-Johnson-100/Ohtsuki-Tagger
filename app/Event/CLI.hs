{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.CLI
  ( module Event.CLI.Type,
    getOptionRecord,
    printVersion,
    cliQuery,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Access (Connection, getFile, lookupFilesHavingFilePattern)
import Database.Tagger.Type
import Event.CLI.Type
import Event.Task
import IO
import System.Exit
import Type.BufferList
import Type.Model.Prim
import Util.Core

getOptionRecord :: [String] -> IO OptionRecord
getOptionRecord args = do
  let (actions, nonOptionErrs, errs) = IO.getOpt IO.RequireOrder optionRecordFlags args
  let finalOptRecord = case actions of
        [] -> baseOptionRecord
        _ -> L.foldl1' (.) actions baseOptionRecord
  mapM_ (IO.hPutStrLn IO.stderr) errs
  mapM_ (IO.hPutStrLn IO.stderr) nonOptionErrs
  return finalOptRecord

printVersion :: OptionRecord -> IO ()
printVersion OptionRecord {optionVersion} = when optionVersion $ putStrLn taggerVersion

-- cliOperateOnFile :: Connection -> OptionRecord -> IO ()
-- cliOperateOnFile c opts@OptionRecord {optionDatabaseFile} =
--   maybe
--     mempty
--     ( \databaseFile -> do
--         _
--     )
--     optionDatabaseFile

-- cliMove :: Connection -> String -> IO ()
-- cliMove c m = case words m of
--   [p, to] -> do
--     x <- runExceptT $ do
--       fs <- lift . lookupFilesHavingFilePattern c . T.pack $ p
--       dbf <-
--         maybeException ("No files found matching pattern, " ++ p) . getFile c
--           <=< maybeException ("No files found matching pattern, " ++ p)
--             . hoistMaybe
--             . head'
--           $ fs ::
--           ExceptT TaskException IO File
--       renameTaggerFile c dbf . T.pack $ to
--     either (const exitFailure <=< print) return x
--   _ ->
--     hPutStrLn
--       stderr
--       "Invalid arguments for move operation, \
--       \expecting two arguments separated by spaces."
--       >> exitFailure

cliQuery :: Connection -> OptionRecord -> IO ()
cliQuery c OptionRecord {optionQuery} =
  maybe
    mempty
    ( \q -> do
        let q' = T.pack q
        r <- runQuery c Union ByTag emptyBufferList q'
        let fs = map (filePath . file) . cCollect $ r
        if null fs
          then hPutStrLn stderr "0 results" >> exitFailure
          else mapM_ T.IO.putStrLn fs
    )
    optionQuery
