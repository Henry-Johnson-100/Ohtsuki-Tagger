{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.Interpreter
Description : Defining various interpreters and annotators for the TaggerQL language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Interpreters allow for the semantics of TaggerQL execution to changed in a simple way.
The definition of an interpreter provides computations for how to compute the leaves
of an expression and how to combine them given the syntactic rules of the language.

Actual evaluation of an expression using any given interpreter is abstracted behind
'runInterpreter`, which takes care of evaluation order.

An expression can be annotated with the same interpreter used to evaluate it, showing
the output of the interpreter at each step of evaluation, along with the expression
that defines it.
-}
module Text.TaggerQL.Expression.Interpreter (
  AnnotatedExpression (..),
  annotation,
  Interpreter (..),
  runInterpreter,
  annotate,

  -- * Interpreters
  queryer,
  voider,
  counter,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State.Strict (State, get, modify)
import Data.Functor.Identity (Identity (Identity))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Ix (Ix)
import Data.Tagger (SetOp (..))
import Database.Tagger (
  File,
  TaggedConnection,
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelationPattern,
  queryForFileByPattern,
 )
import Text.TaggerQL.Expression.AST (
  BinaryExpression (BinaryExpression),
  Expression (..),
  ExpressionIdentity (..),
  ExpressionLeaf (..),
  FileTerm (FileTerm),
  TagTerm (DescriptorTerm, MetaDescriptorTerm),
 )
import Text.TaggerQL.Expression.Interpreter.Internal (
  evalSubExpression,
  queryTags,
  toFileSet,
 )

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
 Interpret the given 'Expression` as if it were @Expression Identity@.
-}
runInterpreter ::
  (Monad m, ExpressionIdentity l) =>
  Interpreter m t ->
  Expression l ->
  m t
runInterpreter (Interpreter ibo iel) = interpret ibo iel . expressionIdentity
 where
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
annotate itr@(Interpreter ibo iel) expr =
  case expressionIdentity expr of
    ExpressionLeaf (Identity leaf) ->
      AnnotatedExpression . ExpressionLeaf . (,leaf) <$> iel leaf
    BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
      lhsI <- annotate itr lhs
      rhsI <- annotate itr rhs
      combination <- ibo so (annotation lhsI) (annotation rhsI)
      return . AnnotatedExpression . BinaryExpressionValue $
        (combination, BinaryExpression (runAnnotation lhsI) so (runAnnotation rhsI))

{- |
 The 'Interpreter` that governs querying a database for a set of 'File`s.

 The very heart and soul of this program.
-}
queryer :: Interpreter (ReaderT TaggedConnection IO) (HashSet File)
queryer =
  Interpreter
    { interpretBinaryOperation =
        \so lhs rhs ->
          pure $
            ( case so of
                Union -> HS.union
                Intersect -> HS.intersection
                Difference -> HS.difference
            )
              lhs
              rhs
    , interpretExpressionLeaf = \leaf -> case leaf of
        FileTermValue (FileTerm t) ->
          ask
            >>= liftIO . fmap HS.fromList . queryForFileByPattern t
        TagTermValue tt ->
          ask
            >>= liftIO . fmap HS.fromList
              . ( case tt of
                    DescriptorTerm txt -> flatQueryForFileByTagDescriptorPattern txt
                    MetaDescriptorTerm txt -> flatQueryForFileOnMetaRelationPattern txt
                )
        TagExpressionValue tt se -> do
          c <- ask
          supertags <- liftIO . fmap HS.fromList $ queryTags tt c
          subExprResult <- evalSubExpression se supertags
          liftIO $ toFileSet subExprResult c
    }

{- |
 Voids an expression.

 Used to easily create a spine-like functor over an 'Expression`

 @
 annotate voider ::
  ExpressionIdentity l => Expression l -> Identity (AnnotatedExpression ())
 @
-}
voider :: Interpreter Identity ()
voider =
  Interpreter
    { interpretBinaryOperation = \_ _ _ -> pure ()
    , interpretExpressionLeaf = \_ -> pure ()
    }

{- |
 Counts the leafs of an 'Expression` by order of evaluation.

 Annotating with this 'Interpreter` creates an 'Expression` where each leaf has an index.
-}
counter :: (Ix a, Num a) => Interpreter (State a) a
counter =
  Interpreter
    { interpretBinaryOperation = \_ _ _ -> get <* modify (+ 1)
    , interpretExpressionLeaf = \_ -> get <* modify (+ 1)
    }