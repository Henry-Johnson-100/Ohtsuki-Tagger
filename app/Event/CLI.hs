{-# OPTIONS_GHC -Wno-typed-holes #-}

module Event.CLI
  (
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Access (Connection)
import Database.Tagger.Type
import Event.CLI.Type
import Event.Task
import qualified IO
import Type.BufferList
import Type.Model.Prim

-- | Return True if there are no errors
showOptErrors :: TaggerOpts -> IO Bool
showOptErrors (TaggerOpts _ _ es) =
  if null es
    then mapM_ (IO.hPutStrLn IO.stderr) es >> return False
    else return True

-- |
-- Runs the supplied query and prints the output filepaths
-- to stdout.
cliQuery :: Connection -> String -> IO ()
cliQuery c qs = do
  let q = T.pack qs
  r <- runQuery c Union ByTag emptyBufferList q
  let fs = map (filePath . file) . cCollect $ r
  mapM_ T.IO.putStrLn fs
