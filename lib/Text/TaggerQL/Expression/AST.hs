{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- |
Module      : Text.TaggerQL.Expression.AST
Description : The syntax tree for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.AST (
  TagTerm (..),
  FileTerm (..),
  SubExpression (..),
  Expression (..),
) where

import Data.String (IsString)
import Data.Tagger (SetOp)
import Data.Text (Text)

{- |
 Data structure representing search terms over the set of 'Descriptor`.
-}
data TagTerm
  = -- | Corresponds to the set of 'Descirptor` that matches the given 'Text`
    DescriptorTerm Text
  | -- | Corresponds to the set of 'Descriptor` that are infra to the 'Descriptor`
    -- matching the given 'Text`, including the matches themselves.
    MetaDescriptorTerm Text
  deriving (Show, Eq)

{- |
 Corresponds to a search term over a filepath.
-}
newtype FileTerm
  = FileTerm Text
  deriving (Show, Eq, Semigroup, Monoid, IsString)

{- |
 A 'SubExpression` is a structure that defines a set of 'Tag` that is some
 subset of the set of all 'Tag` in the database.

 A 'SubExpression` is then used:
  * as an operand in another 'SubExpression`
  * to compute a set of 'File` in an 'Expression`
  * to tag a single 'File` with the set of 'Tag` that the 'SubExpression` represents.
-}
data SubExpression
  = SubTag TagTerm
  | SubBinary SubExpression SetOp SubExpression
  | SubExpression TagTerm SubExpression
  deriving (Show, Eq)

{- |
 An 'Expression` is a structure that defines a set of 'File` that is some subset of
 the set of all 'File` in the database.

 An 'Expression` is a complete TaggerQL query.
-}
data Expression
  = FileTermValue FileTerm
  | TagTermValue TagTerm
  | -- | Constructs a 'Tag` set from the given 'TagTerm`
    -- that serves as the inital environment for the given 'SubExpression`.
    --
    -- Essentially, defines the set of 'File` where 'SubExpression` are subtags
    -- of any 'Tag` appearing in the set defined by the 'TagTerm`.
    TagExpression TagTerm SubExpression
  | -- | A binary operation over two 'Expression`
    Binary Expression SetOp Expression
  deriving (Show, Eq)
