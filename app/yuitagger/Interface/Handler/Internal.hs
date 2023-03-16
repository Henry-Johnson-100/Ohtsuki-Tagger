{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Interface.Handler.Internal (
  addFiles,
  shuffleSequence,
  runShellCmd,
) where

import Control.Monad ((<=<), (>=>))
import Data.Hashable (Hashable, hashWithSalt)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (
  hGetContents,
  hPrint,
  hPutStrLn,
  stderr,
  stdout,
 )
import System.Process (
  CreateProcess (delegate_ctlc, new_session),
  createProcess,
  proc,
  waitForProcess,
 )
import System.Random (Random (random), StdGen, initStdGen)
import Util (addFiles)

runShellCmd :: Text -> [FilePath] -> IO ()
runShellCmd cmdString files =
  case T.unpack <$> T.words cmdString of
    (c : args) -> do
      let cmd =
            (proc c (args ++ files))
              { delegate_ctlc = True
              , new_session = True
              }
      p <- createProcess cmd
      let pout = (\(_, h, _, _) -> h) p
          perr = (\(_, _, h, _) -> h) p
          pProc = (\(_, _, _, p') -> p') p
      hReadMaybe stdout pout
      hReadMaybe stderr perr
      hPrint stderr <=< waitForProcess $ pProc
    [] -> return ()
 where
  hReadMaybe oh mh =
    maybe (pure ()) (hGetContents >=> hPutStrLn oh) mh

{- |
 Sorts a given sequence with a random seed.
-}
shuffleSequence :: Hashable a => Seq a -> IO (Seq a)
shuffleSequence s = do
  !shuffleSeed <- initStdGen :: IO StdGen
  let genFileHash = hashWithSalt (fst . random $ shuffleSeed)
      !sortedSeq = Seq.unstableSortOn genFileHash s
  return sortedSeq
