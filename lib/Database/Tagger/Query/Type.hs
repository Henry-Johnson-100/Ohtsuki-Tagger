{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- |
Module      : Database.Tagger.Query.Type
Description : Types for different kinds of Queries.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger.Query.Type (
  TaggerQuery (..),
  qcat,
  paramList,
) where

import qualified Data.List as L
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

{- |
 Given a number of parameters, generate a literal 'TaggerQuery` list of positional
 SQLite parameters.

 > paramList 5 == "(?,?,?,?,?)"
-}
paramList :: Int -> TaggerQuery
paramList n = "(" <> (fromString . L.intercalate "," . replicate n $ "?") <> ")"