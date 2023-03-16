{- |
Module      : YuiTagger.Info
Description : Basic information detailing runtime information of tagger-lib

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module YuiTagger.Info (
  yuiTaggerVersion,
) where

import qualified Data.Version as Version
import qualified Paths_yuitagger

{-# INLINE yuiTaggerVersion #-}
yuiTaggerVersion :: Version.Version
yuiTaggerVersion = Paths_yuitagger.version
