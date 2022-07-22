{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Text.TaggerQL.Compiler
Description : Module that compiles a TaggerQL AST to a SQLite query.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

The compiler specifically works ONLY on the 'TermTree` struct and below.
There is no need to compile any higher level structures to SQLite as they are all
treated as distinct queries and can be combined and aggregated appopriately in memory.
-}
module Text.TaggerQL.Compiler () where

import Data.HashMap.Strict (HashMap)
import Data.String (IsString)
import Data.Tagger
import Data.Text (Text)
import Database.Tagger.Query.Type
import Text.RawString.QQ (r)
import Text.TaggerQL.AST

data ParamMap = ParamMap
  { paramMapIter :: Int
  , paramMap :: HashMap Text Int
  }
  deriving (Show, Eq)

data TaggerQLCompilerTarget = TaggerQLCompilerTarget
  { taggerQLCompilerQuery :: TaggerQuery
  , taggerQLCompilerParams :: [Text]
  }
  deriving (Show, Eq)

newtype UnterminatedNTypeQuery = UnterminatedNTypeQuery TaggerQLCompilerTarget
  deriving (Show, Eq)

newtype FileJoinTerminatedQuery = FileJoinTerminatedQuery TaggerQLCompilerTarget
  deriving (Show, Eq)

compileTermTree :: TermTree Text -> FileJoinTerminatedQuery
compileTermTree = undefined

-- fileJoinTermination :: UnterminatedNTypeQuery -> FileJoinTerminatedQuery
-- fileJoinTermination (UnterminatedNTypeQuery (TaggerQLCompilerTarget q ps)) =
--   FileJoinTerminatedQuery . flip TaggerQLCompilerTarget ps $
--     "WITH file_terminal AS ("
--       `qcat` q
--       `qcat` ")"
--       `qcat` [r|
-- f.id
-- ,f.filePath
-- FROM n
-- JOIN File f
--   ON file_terminal.fileId = f.id|]