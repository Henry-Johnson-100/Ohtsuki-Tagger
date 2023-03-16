{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- |
Module      : Database.YuiTagger.Query.Type
Description : Types for different kinds of Queries.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.YuiTagger.Query.Type (
  TaggerQuery (..),
  qcat,
) where

import Data.String
import Database.SQLite.Simple

{- |
 newtype wrapper for a Database.SQLite.Simple.Query type
-}
newtype TaggerQuery = TaggerQuery {taggerQuery :: Query}
  deriving
    ( Show
    , Eq
    , Monoid
    , Semigroup
    , IsString
    )

{- |
 Concatenate two 'TaggerQuery`s together with a single space between them
 to prevent syntax errors from combined keywords or search terms when combining
 queries.
-}
qcat :: TaggerQuery -> TaggerQuery -> TaggerQuery
x `qcat` y = x <> " " <> y
