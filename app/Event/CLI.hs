{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.CLI
  ( module Event.CLI.Type,
    getOptionRecord,
    printVersion,
    printHelp,
    cliQuery,
    cliOperateOnFile,
  )
where

import Control.Monad (when, (<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Access (Connection, getFile, lookupFilesHavingFilePattern)
import Database.Tagger.Type (File (filePath), FileWithTags (file))
import Event.CLI.Type
import Event.Task (removeTaggerFile, renameTaggerFile, runQuery)
import IO
  ( ArgOrder (RequireOrder),
    getOpt,
    hPrint,
    hPutStrLn,
    maybeException,
    stderr,
    taggerVersion,
    usageInfo,
    whenJust,
  )
import System.Exit (exitFailure)
import Type.BufferList (Cycleable (cCollect), emptyBufferList)
import Type.Model.Prim
  ( FileSetArithmetic (Union),
    QueryCriteria (ByTag),
  )

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

printHelp :: OptionRecord -> IO ()
printHelp OptionRecord {optionHelp} =
  when optionHelp . putStrLn
    . usageInfo
      "Tagger is a GUI file tagger and database querying program.\n\
      \Some operations can be performed via CLI.\n\
      \Any option specified with (no-run) will cause the GUI to not launch.\n\
      \Running with no options or only options \
      \not marked with (no-run) will launch the GUI.\n\
      \USAGE:\ttagger [options]"
    $ optionRecordFlags

-- |
-- Perform operations on the fixed databasefile given with -f.
-- If a unique file is found then a sequence of possible actions given the OptionRecord
-- is performed, otherwise an error is printed.
cliOperateOnFile :: Connection -> OptionRecord -> IO ()
cliOperateOnFile c opts@OptionRecord {optionDatabaseFile} =
  IO.whenJust
    optionDatabaseFile
    ( \databaseFile ->
        do
          uniqueDatabaseFile <- runExceptT . cliGetUniqueDatabaseFile $ databaseFile
          either
            (const exitFailure <=< IO.hPrint IO.stderr)
            ( \f ->
                F.sequenceA_ $
                  [cliRemoveFile c f, cliRenameTaggerFile c f] <*> [opts]
            )
            uniqueDatabaseFile
    )
  where
    cliGetUniqueDatabaseFile :: String -> ExceptT OptionException IO File
    cliGetUniqueDatabaseFile databaseFile =
      do
        fs <- lift $ lookupFilesHavingFilePattern c . T.pack $ databaseFile
        case fs of
          [f] ->
            maybeException
              "Unable to retrieve file from filekey, \
              \though the given pattern corresponds to exactly one file. \
              \Something is very wrong for this to happen"
              $ getFile c f
          [] ->
            throwE
              ( OptionException
                  "Given pattern corresponds to 0 files in the database."
              )
          _ ->
            throwE
              ( OptionException
                  "Given pattern corresponds to more than one file in the database. \
                  \Please provide a pattern to identify one unique file."
              )

    cliRenameTaggerFile :: Connection -> File -> OptionRecord -> IO ()
    cliRenameTaggerFile c' f OptionRecord {optionMove} =
      IO.whenJust
        optionMove
        ( \moveTo -> do
            e <- runExceptT $ renameTaggerFile c' f (T.pack moveTo)
            either (IO.hPrint IO.stderr) (const mempty) e
        )

    cliRemoveFile :: Connection -> File -> OptionRecord -> IO ()
    cliRemoveFile c' f OptionRecord {optionRemove} = when optionRemove $ do
      e <- runExceptT $ removeTaggerFile c' f
      either (const exitFailure <=< IO.hPrint IO.stderr) return e

cliQuery :: Connection -> OptionRecord -> IO ()
cliQuery c OptionRecord {optionQuery} =
  IO.whenJust
    optionQuery
    ( \q -> do
        let q' = T.pack q
        r <- runQuery c Union ByTag emptyBufferList q'
        let fs = map (filePath . file) . cCollect $ r
        if null fs
          then hPutStrLn stderr "0 results" >> exitFailure
          else mapM_ T.IO.putStrLn fs
    )
