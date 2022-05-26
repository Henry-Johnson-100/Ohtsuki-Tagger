{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Event.CLI
  ( module Event.CLI.Type,
    showOptErrors,
    cliQuery,
    getTaggerOpt,
    nullOpts,
    hasQueryFlag,
    getQuery,
    runOpt,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
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

-- | Return True if there are errors
showOptErrors :: TaggerOpts -> IO Bool
showOptErrors (TaggerOpts _ ns es) = do
  unless
    (null ns)
    ( IO.hPutStrLn IO.stderr "Non-options:"
        >> mapM_ (IO.hPutStrLn IO.stderr . (++) "\t") ns
    )
  unless
    (null es)
    ( IO.hPutStrLn IO.stderr "Option errors:"
        >> mapM_ (IO.hPutStrLn IO.stderr . (++) "\t") es
    )
  return . or $ not . null <$> [ns, es]

runOpt :: Connection -> CLIFlag -> IO ()
runOpt c f =
  case f of
    Version -> putStrLn taggerVersion
    Query q -> cliQuery c q
    Move m -> cliMove c m

cliMove :: Connection -> String -> IO ()
cliMove c m = case words m of
  [p, to] -> do
    x <- runExceptT $ do
      fs <- lift . lookupFilesHavingFilePattern c . T.pack $ p
      dbf <-
        maybeException ("No files found matching pattern, " ++ p) . getFile c
          <=< maybeException ("No files found matching pattern, " ++ p)
            . hoistMaybe
            . head'
          $ fs ::
          ExceptT TaskException IO File
      renameTaggerFile c dbf . T.pack $ to
    either (const exitFailure <=< print) return x
  _ ->
    hPutStrLn
      stderr
      "Invalid arguments for move operation, \
      \expecting two arguments separated by spaces."
      >> exitFailure

-- |
-- Runs the supplied query and prints the output filepaths
-- to stdout.
cliQuery :: Connection -> String -> IO ()
cliQuery c qs = do
  let q = T.pack qs
  r <- runQuery c Union ByTag emptyBufferList q
  let fs = map (filePath . file) . cCollect $ r
  if null fs
    then hPutStrLn stderr "0 results" >> exitFailure
    else mapM_ T.IO.putStrLn fs

getTaggerOpt :: [String] -> TaggerOpts
getTaggerOpt = uncurryOpts . IO.getOpt IO.RequireOrder cliFlags
  where
    uncurryOpts :: ([CLIFlag], [String], [String]) -> TaggerOpts
    uncurryOpts (cs, ns, es) = TaggerOpts cs ns es

nullOpts :: TaggerOpts -> Bool
nullOpts (TaggerOpts [] [] []) = True
nullOpts _ = False

hasQueryFlag :: TaggerOpts -> Bool
hasQueryFlag = any (\f -> case f of Query _ -> True; _ -> False) . optionArguments

getQuery :: TaggerOpts -> Maybe String
getQuery opts =
  if not . hasQueryFlag $ opts
    then Nothing
    else getQuery' . optionArguments $ opts
  where
    getQuery' [] = Nothing
    getQuery' (Query s : _) = Just s
    getQuery' (_ : xs) = getQuery' xs
