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

{-# HLINT ignore "Use lambda-case" #-}

{- |
Module      : Text.TaggerQL.Expression.AST
Description : The syntax tree for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.AST (
  -- * AST
  Expression (..),
  ExpressionLeaf (..),
  BinaryExpression (..),
  SubExpression (..),
  TagTerm (..),
  FileTerm (..),

  -- * Classes
  ExpressionIdentity (..),

  -- * Annotations
  -- $Annotations
  annotate,
  annotateIndices,

  -- * Constants
  zero,
  universe,
) where

import Control.Monad.Trans.State.Strict (evalState, get, modify)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (Identity))
import Data.Ix (Ix)
import Data.String (IsString)
import Data.Tagger (SetOp (..))
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
 Intermediate constructor for lifting a binary set operation over 'Expression` into
 an 'Expression`
-}
data BinaryExpression t = BinaryExpression (Expression t) SetOp (Expression t)

deriving instance Show (BinaryExpression Identity)

deriving instance Eq (BinaryExpression Identity)

deriving instance Show a => Show (BinaryExpression ((,) a))

deriving instance Eq a => Eq (BinaryExpression ((,) a))

{- |
 Intermediate constructor denoting the terminal evaluations of an 'Expression`
-}
data ExpressionLeaf
  = FileTermValue FileTerm
  | TagTermValue TagTerm
  | -- | Constructs a 'Tag` set from the given 'TagTerm`
    -- that serves as the inital environment for the given 'SubExpression`.
    --
    -- Essentially, defines the set of 'File` where 'SubExpression` are subtags
    -- of any 'Tag` appearing in the set defined by the 'TagTerm`.
    TagExpressionValue TagTerm SubExpression
  deriving (Show, Eq)

{- |
 An 'Expression` is a structure that defines a set of 'File` that is some subset of
 the set of all 'File` in the database.

 'Expression` is a higher kinded type. This additional type can be used to annotate the
 leaves of an 'Expression` tree.

 An 'Expression` is a complete TaggerQL query.
-}
data Expression t
  = ExpressionLeaf (t ExpressionLeaf)
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
    ExpressionLeaf l -> ExpressionLeaf . Identity . snd $ l
    BinaryExpressionValue (_, BinaryExpression lhs so rhs) ->
      BinaryExpressionValue . Identity $
        BinaryExpression
          (expressionIdentity lhs)
          so
          (expressionIdentity rhs)

{- |
 Annotate each leaf of an 'Expression` in the order it gets evaluated.

 0 indexed.
-}
annotateIndices :: (Num a, Ix a) => Expression Identity -> Expression ((,) a)
annotateIndices =
  flip evalState 0
    . annotate
      (\_ _ -> get)
      (\_ _ -> get)
      (\_ _ -> get)
      (,)
      (\expr -> modify (+ 1) $> expr)
      (\leaf -> fmap (ExpressionLeaf . (,leaf)) get <* modify (+ 1))

{- $Annotations
 Expression can be annotated using 'annotate` to supplement the leaf of each 'Expression`
 with some additional information. Below is an example showing how 'annotate` is used
 to create an index of each leaf of an expression in the order that it is evaluated.

 @
annotateIndices :: (Num a, Ix a) => Expression Identity -> Expression ((,) a)
annotateIndices =
  flip evalState 0
    . annotate
      (\\_ _ -> get)
      (\\_ _ -> get)
      (\\_ _ -> get)
      (,)
      (\\expr -> modify (+ 1) $> expr)
      (\\leaf -> fmap (ExpressionLeaf . (,leaf)) get <* modify (+ 1))
 @

This function uses the 'StateT` monad to keep track of the current index.
-}

{- |
 Generalized traversal of an 'Expression Identity`.

 Runs on the 'Expression` in evaluation order, annotating its leaves as it goes.
-}
annotate ::
  Monad m =>
  -- | Result when 'Union` is called on two 'Expression`
  (Expression t1 -> Expression t1 -> m t2) ->
  -- | Result when 'Intersect` is called on two 'Expression`
  (Expression t1 -> Expression t1 -> m t2) ->
  -- | Result when 'Difference` is called on two 'Expression`
  (Expression t1 -> Expression t1 -> m t2) ->
  -- | Uses the result of one of the above functions to annotate a 'BinaryExpression`
  -- with some intermediate annotation
  (t2 -> BinaryExpression t1 -> t3 (BinaryExpression t3)) ->
  -- | lifts the above intermediate annotation to the context of the traversal
  (Expression t3 -> m (Expression t1)) ->
  -- | Annotate an 'ExpressionLeaf`
  (ExpressionLeaf -> m (Expression t1)) ->
  -- | The 'Expression` to annotate
  Expression Identity ->
  m (Expression t1)
annotate
  unionF
  intersectF
  differenceF
  liftBinaryExpressionWith
  returnBinary
  onLeaf
  expr =
    case expr of
      ExpressionLeaf (Identity leaf) -> onLeaf leaf
      BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
        lhsLifted <- annotate' lhs
        rhsLifted <- annotate' rhs
        combination <- lhsLifted `combFunc` rhsLifted
        returnBinary
          ( BinaryExpressionValue . liftBinaryExpressionWith combination $
              BinaryExpression lhsLifted so rhsLifted
          )
       where
        combFunc = case so of
          Union -> unionF
          Intersect -> intersectF
          Difference -> differenceF
        annotate' =
          annotate
            unionF
            intersectF
            differenceF
            liftBinaryExpressionWith
            returnBinary
            onLeaf

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
                    ( ExpressionLeaf
                        . Identity
                        . TagTermValue
                        . DescriptorTerm
                        . T.pack
                        $ "%"
                    )
              )
          )
          Intersect
          (ExpressionLeaf . Identity . TagTermValue . DescriptorTerm . T.pack $ "%")
    )

{-# INLINE universe #-}

{- |
 This 'Expression` will always evaluate to all files in the database.
-}
universe :: Expression Identity
universe = ExpressionLeaf . Identity . FileTermValue . FileTerm . T.pack $ "%"
