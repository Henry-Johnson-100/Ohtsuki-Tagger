{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_HADDOCK hide, ignore-exports, prune #-}

{- |
Module      : Database.Tagger.Type.Prim
Description : Types that have lenses defined in "Database.Tagger.Type.Lens"

License     : GPL-3
Maintainer  : monawasensei@gmail.com

The types defined here all have lenses defined in the module "Database.Tagger.Type.Lens".
The two modules are separated purely to make exporting lenses and lens instances together
with non-TemplateHaskell generated code simpler.
-}
module Database.Tagger.Type.Prim (
  BareConnection,
  TaggedConnection (..),
  _bareConnection,
) where

import Data.Text
import Database.SQLite.Simple

-- * Connection Types

{- |
 A newtype that wraps a 'Connection` to create instances of Show and Eq.
-}
newtype BareConnection = BareConnection {_bareConnection :: Connection}

instance Show BareConnection where
  show = const "Bare Connection"

instance Eq BareConnection where
  _ == _ = True

{- |
 A data type that manages connections to a Tagger database.
-}
data TaggedConnection = TaggedConnection
  { -- | Connection name,
    -- typically the absolute path of the file connected to, but could be anything.
    _taggedconnectionConnName :: !Text
  , -- | Connection to a database, if it is open.
    _taggedconnectionConnInstance :: !(Maybe BareConnection)
  }
  deriving (Eq, Show)
