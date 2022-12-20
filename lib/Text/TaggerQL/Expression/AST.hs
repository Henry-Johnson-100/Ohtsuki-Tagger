{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ViewPatterns #-}
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
  -- $Annotators
  Annotator (..),
  runAnnotator,
  annotate,
  annotateIndices,

  -- * Constants
  zero,
  universe,
) where

import Control.Monad.Trans.State.Strict (State, evalState, get, modify)
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

{- $Annotators
 An 'Annotator` provides a generic way to describe an annotation of any
 @ExpressionIdentity t => Expression t@.

 It traverses the 'Expression` in order of evaluation, dispatching the appropriate
 function.

 Its first 3 functions, 'annotateUnion`, 'annotateIntersection`, and 'annotateDifference`
 can be classified for any ring 'finalAnnotation a`
 where union is addition, intersection is multiplication and difference
 is the additive inverse.

 But the provided implementation is generic, allowing flexible use for any context.
 An example of this is the 'indexAnnotator` that provides an index for each 'Expression`
 by order of its evaluation:

 @
indexAnnotator :: (Ix a, Num a) => Annotator (State a) a ((,) a) ((,) a)
indexAnnotator =
  Annotator
    { annotateUnion = combGet
    , annotateIntersection = combGet
    , annotateDifference = combGet
    , annotateBinaryExpression = (,)
    , liftBinaryExpression = \\binExpr -> modify (+ 1) $> binExpr
    , liftLeaf = \\leaf -> fmap (,leaf) get <* modify (+ 1)
    }
 where
  combGet _ _ = get
 @
-}

{- |
 Data type that describes some generic behavior when annotating an 'Expression`.
-}
data Annotator m setOpResult binaryIntermediate finalAnnotation = Annotator
  { -- | Union operation
    annotateUnion ::
      Expression finalAnnotation ->
      Expression finalAnnotation ->
      m setOpResult
  , -- | Intersection operation
    annotateIntersection ::
      Expression finalAnnotation ->
      Expression finalAnnotation ->
      m setOpResult
  , -- | Difference operation
    annotateDifference ::
      Expression finalAnnotation ->
      Expression finalAnnotation ->
      m setOpResult
  , -- | Function describing how to use the value provided by one of the
    -- combination functions above to annotate the binary expression.
    --
    -- This function may result in an intermediate kind 'binaryIntermediate` that must
    -- be finally resolved by the 'liftBinaryExpression` function.
    --
    -- But typically, the type of 'binaryIntermediate` and 'finalAnnotation` are the same.
    annotateBinaryExpression ::
      setOpResult ->
      BinaryExpression finalAnnotation ->
      binaryIntermediate (BinaryExpression binaryIntermediate)
  , -- | Function describing how to lift an annotated binary
    -- expression into the final context
    liftBinaryExpression ::
      binaryIntermediate (BinaryExpression binaryIntermediate) ->
      m (finalAnnotation (BinaryExpression finalAnnotation))
  , -- | Function describing how to annotate the terminal leaves of an 'Expression`
    liftLeaf :: ExpressionLeaf -> m (finalAnnotation ExpressionLeaf)
  }

runAnnotator ::
  (ExpressionIdentity l, Monad m) =>
  Annotator m k b t ->
  Expression l ->
  m (Expression t)
runAnnotator (Annotator u i d ab lb ll) =
  annotate u i d ab lb ll . expressionIdentity

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
  (t3 (BinaryExpression t3) -> m (t1 (BinaryExpression t1))) ->
  -- | Annotate an 'ExpressionLeaf`
  (ExpressionLeaf -> m (t1 ExpressionLeaf)) ->
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
      ExpressionLeaf (Identity leaf) -> fmap ExpressionLeaf . onLeaf $ leaf
      BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
        lhsLifted <- annotate' lhs
        rhsLifted <- annotate' rhs
        combination <- lhsLifted `combFunc` rhsLifted
        fmap BinaryExpressionValue
          . returnBinary
          . liftBinaryExpressionWith combination
          $ BinaryExpression lhsLifted so rhsLifted
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

{- |
 Annotate each leaf of an 'Expression` in the order it gets evaluated.

 0 indexed.
-}
annotateIndices :: (Num a, Ix a) => Expression Identity -> Expression ((,) a)
annotateIndices = flip evalState 0 . runAnnotator indexAnnotator

indexAnnotator :: (Ix a, Num a) => Annotator (State a) a ((,) a) ((,) a)
indexAnnotator =
  Annotator
    { annotateUnion = combGet
    , annotateIntersection = combGet
    , annotateDifference = combGet
    , annotateBinaryExpression = (,)
    , liftBinaryExpression = \binExpr -> modify (+ 1) $> binExpr
    , liftLeaf = \leaf -> fmap (,leaf) get <* modify (+ 1)
    }
 where
  combGet _ _ = get

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
