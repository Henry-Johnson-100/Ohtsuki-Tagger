{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

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

import Data.Tagger
import Data.Text (Text)
import Database.Tagger.Query.Type
import Text.TaggerQL.AST

data TaggerQLCompilerTarget = TaggerQLCompilerTarget
  { taggerQLCompilerQuery :: TaggerQuery
  , taggerQLCompilerParams :: [Text]
  }
  deriving (Show, Eq)

-- compileTermTree :: TermTree Text -> TaggerQuery