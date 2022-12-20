{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
  TagExpression (..),
  BinaryExpression (..),
  Expression (..),

  -- * Constants
  zero,
  universe,
) where

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity (Identity (Identity))
import Data.Ix (Ix)
import Data.String (IsString)
import Data.Tagger (SetOp (Difference, Intersect))
import Data.Text (Text)
import qualified Data.Text as T

{- |
 Data structure representing search terms over the set of 'Descriptor`.
-}
data TagTerm
  = -- | Corresponds to the set of `Descriptor' that matches the given 'Text`
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
  = -- | A search term for a set of 'Tag` that are subtags in the current environment.
    SubTag TagTerm
  | SubBinary SubExpression SetOp SubExpression
  | -- | Extends the current 'Tag` environment through the given 'TagTerm`
    -- to define a more constrained set with the given 'SubExpression`.
    SubExpression TagTerm SubExpression
  deriving (Show, Eq)

{- |
 Intermediate constructor for lifting a 'SubExpression` into an 'Expression` with a
 'TagTerm`.
-}
data TagExpression = TagExpression TagTerm SubExpression
  deriving (Show, Eq)

data BinaryExpression t = BinaryExpression (Expression t) SetOp (Expression t)

deriving instance Show (BinaryExpression Identity)

deriving instance Eq (BinaryExpression Identity)

deriving instance Show a => Show (BinaryExpression ((,) a))

deriving instance Eq a => Eq (BinaryExpression ((,) a))

{- |
 An 'Expression` is a structure that defines a set of 'File` that is some subset of
 the set of all 'File` in the database.

 An 'Expression` is a complete TaggerQL query.
-}
data Expression t
  = FileTermValue (t FileTerm)
  | TagTermValue (t TagTerm)
  | -- | Constructs a 'Tag` set from the given 'TagTerm`
    -- that serves as the inital environment for the given 'SubExpression`.
    --
    -- Essentially, defines the set of 'File` where 'SubExpression` are subtags
    -- of any 'Tag` appearing in the set defined by the 'TagTerm`.
    TagExpressionValue (t TagExpression)
  | BinaryExpressionValue (t (BinaryExpression t))

deriving instance Show (Expression Identity)

deriving instance Eq (Expression Identity)

deriving instance Show a => Show (Expression ((,) a))

deriving instance Eq a => Eq (Expression ((,) a))

{- |
 A class for an 'Expression t` that is possible to transform to
 an 'Expression Identity`
-}
class ExpressionIdentity t where
  expressionIdentity :: Expression t -> Expression Identity

instance ExpressionIdentity Identity where
  expressionIdentity :: Expression Identity -> Expression Identity
  expressionIdentity = id

instance ExpressionIdentity ((,) a) where
  expressionIdentity :: Expression ((,) a) -> Expression Identity
  expressionIdentity expr = case expr of
    FileTermValue x0 -> FileTermValue . Identity . snd $ x0
    TagTermValue x0 -> TagTermValue . Identity . snd $ x0
    TagExpressionValue x0 -> TagExpressionValue . Identity . snd $ x0
    BinaryExpressionValue (_, BinaryExpression lhs so rhs) ->
      BinaryExpressionValue . Identity $
        BinaryExpression
          (expressionIdentity lhs)
          so
          (expressionIdentity rhs)

{- |
 Index an 'Expression` by its evaluation order.
-}
buildIndexedExpression :: (Ix a, Num a) => Expression Identity -> Expression ((,) a)
buildIndexedExpression expr = evalState (go expr) 0
 where
  go :: (Ix a, Num a) => Expression Identity -> State a (Expression ((,) a))
  go e =
    case e of
      FileTermValue (Identity ft) -> fmap (FileTermValue . (,ft)) get <* modify (+ 1)
      TagTermValue (Identity tte) -> fmap (TagTermValue . (,tte)) get <* modify (+ 1)
      TagExpressionValue (Identity tev) ->
        fmap (TagExpressionValue . (,tev)) get <* modify (+ 1)
      BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) ->
        ( ( \lhsIx rhsIx soSt ->
              BinaryExpressionValue
                (soSt, BinaryExpression lhsIx so rhsIx)
          )
            <$> go lhs
            <*> go rhs
            <*> get
        )
          <* modify (+ 1)

{-# INLINE zero #-}

{- |
 This 'Expression` will always evaluate to an empty set.

 It is the intersection of tagged and untagged files.
-}
zero :: Expression Identity
zero =
  BinaryExpressionValue
    ( Identity $
        BinaryExpression
          ( BinaryExpressionValue
              ( Identity $
                  BinaryExpression
                    universe
                    Difference
                    (TagTermValue . Identity . DescriptorTerm . T.pack $ "%")
              )
          )
          Intersect
          (TagTermValue . Identity . DescriptorTerm . T.pack $ "%")
    )

{-# INLINE universe #-}

{- |
 This 'Expression` will always evaluate to all files in the database.
-}
universe :: Expression Identity
universe = FileTermValue . Identity . FileTerm . T.pack $ "%"
