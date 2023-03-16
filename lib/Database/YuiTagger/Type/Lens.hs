{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune, hide #-}

{- |
Module      : Database.YuiTagger.Type.Lens

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.YuiTagger.Type.Lens (
  module Database.YuiTagger.Type.Lens,
) where

import Lens.Micro.TH (
  abbreviatedFields,
  makeLenses,
  makeLensesWith,
 )
import Database.YuiTagger.Type.Prim (
  BareConnection,
  TaggedConnection,
 )

makeLensesWith abbreviatedFields ''TaggedConnection

makeLenses ''BareConnection
