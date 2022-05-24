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
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Access (Connection)
import Database.Tagger.Type
import Event.CLI.Type
import Event.Task
import qualified IO
import Type.BufferList
import Type.Model.Prim

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

-- |
-- Runs the supplied query and prints the output filepaths
-- to stdout.
cliQuery :: Connection -> String -> IO ()
cliQuery c qs = do
  let q = T.pack qs
  r <- runQuery c Union ByTag emptyBufferList q
  let fs = map (filePath . file) . cCollect $ r
  mapM_ T.IO.putStrLn fs

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
