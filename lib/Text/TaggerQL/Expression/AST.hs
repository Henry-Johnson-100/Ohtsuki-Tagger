{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use lambda-case" #-}
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
  AnnotatedExpression (..),
  annotation,
  Expression (..),
  ExpressionLeaf (..),
  BinaryExpression (..),
  SubExpression (..),
  TagTerm (..),
  FileTerm (..),

  -- * Classes
  ExpressionIdentity (..),

  -- * Interpreters
  Interpreter (..),
  runInterpreter,
  annotate,

  -- * Constants
  zero,
  universe,
) where

import Data.Functor.Identity (Identity (..))
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
 newtype wrapper for an 'Expression ((,) a)`.

 denoting some auxiliary information at each leaf in the 'Expression`
-}
newtype AnnotatedExpression a = AnnotatedExpression {runAnnotation :: Expression ((,) a)}
  deriving (Show, Eq)

instance Functor AnnotatedExpression where
  fmap :: (a -> b) -> AnnotatedExpression a -> AnnotatedExpression b
  fmap f (AnnotatedExpression expr) = AnnotatedExpression $ case expr of
    ExpressionLeaf (x, i) -> ExpressionLeaf (f x, i)
    BinaryExpressionValue
      ( x
        , BinaryExpression
            lhs
            so
            rhs
        ) ->
        BinaryExpressionValue
          ( f x
          , BinaryExpression
              (runAnnotation . fmap f . AnnotatedExpression $ lhs)
              so
              (runAnnotation . fmap f . AnnotatedExpression $ rhs)
          )

{- |
 Return the given 'Expression` annotation.
-}
annotation :: AnnotatedExpression a -> a
annotation (AnnotatedExpression expr) = case expr of
  ExpressionLeaf x0 -> fst x0
  BinaryExpressionValue x0 -> fst x0

{- |
 A data type containing behavior for how to interpret an 'Expression`.

 Where 'm` is the context to interpret in, typically some monad, and
 t is the final output of the interpreter.
-}
data Interpreter m t = Interpreter
  { interpretBinaryOperation :: SetOp -> t -> t -> m t
  , interpretExpressionLeaf :: ExpressionLeaf -> m t
  }

{- |
 Interpret the given 'Expression` as if it were 'Expression Identity`.
-}
runInterpreter ::
  (Monad m, ExpressionIdentity l) =>
  Interpreter m t ->
  Expression l ->
  m t
runInterpreter (Interpreter ibo iel) = interpret ibo iel . expressionIdentity

{- |
 Given any 'Interpreter` i and any 'Expression` e, then the following should (hopefully)
 be true:

 >interpret i e = fmap annotation (annotate i e)
-}
annotate ::
  (Monad m, ExpressionIdentity l) =>
  Interpreter m t ->
  Expression l ->
  m (AnnotatedExpression t)
annotate itr@(Interpreter ibo iel) expr = case expressionIdentity expr of
  ExpressionLeaf (Identity leaf) ->
    AnnotatedExpression . ExpressionLeaf . (,leaf) <$> iel leaf
  BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
    lhsI <- annotate itr lhs
    rhsI <- annotate itr rhs
    combination <- ibo so (annotation lhsI) (annotation rhsI)
    return . AnnotatedExpression . BinaryExpressionValue $
      (combination, BinaryExpression (runAnnotation lhsI) so (runAnnotation rhsI))

{- |
 A generic interpreter over an 'Expression Identity`

 Order of operation and function dispatch is handled by the interpreter.
-}
interpret ::
  Monad m =>
  (SetOp -> t -> t -> m t) ->
  (ExpressionLeaf -> m t) ->
  Expression Identity ->
  m t
interpret
  dispatchComb
  onLeaf
  expr =
    case expr of
      ExpressionLeaf (Identity l) -> onLeaf l
      BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
        lhsI <- interpret' lhs
        rhsI <- interpret' rhs
        dispatchComb so lhsI rhsI
       where
        interpret' =
          interpret
            dispatchComb
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
