{- |
Module      : Tagger.Info
Description : Basic information detailing runtime information of tagger-lib

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Tagger.Info (
  taggerVersion,
) where

import qualified Data.Version as Version
import qualified Paths_tagger

{- |
 Returns Tagger's current version string.
-}
taggerVersion :: String
taggerVersion = Version.showVersion Paths_tagger.version