{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Database.Tagger.Query.Type
Description : Types for different kinds of Queries.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger.Query.Type (
  TaggerQuery (..),
) where

import Data.String (IsString)
import Database.SQLite.Simple (Query (Query))

{- |
 newtype wrapper for a Database.SQLite.Simple.Query type
-}
newtype TaggerQuery = TaggerQuery {taggerQuery :: Query}
  deriving
    ( Show
    , Eq
    , Semigroup
    , Monoid
    , IsString
    )