{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune, hide #-}

{- |
Module      : Database.Tagger.Type.Lens

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger.Type.Lens (
  module Database.Tagger.Type.Lens,
) where

import Control.Lens (
  abbreviatedFields,
  makeLenses,
  makeLensesWith,
 )
import Database.Tagger.Type.Prim (
  BareConnection,
  TaggedConnection,
 )

makeLensesWith abbreviatedFields ''TaggedConnection

makeLenses ''BareConnection
