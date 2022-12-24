{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}

{- |
Module      : Text.TaggerQL.Expression.Interpreter
Description : Defining various interpreters and annotators for the TaggerQL language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Interpreters allow for the semantics of TaggerQL execution to be changed in a simple way.
The definition of an interpreter provides computations for how to compute the leaves
of an expression and how to combine them given the syntactic rules of the language.

Actual evaluation of an expression using any given interpreter is abstracted behind
'runInterpreter`, which takes care of evaluation order.

An expression can be annotated with the same interpreter used to evaluate it, showing
the output of the interpreter at each step of evaluation, along with the expression
that defines it.

Simply put, interpretation of an expression is just a fold in some monad. An annotation
is a traversal.

Intepreters have lots of resemblances to Functors, though their internal computations
are stateful over their inner type, making a proper Functor instance difficult.
-}
module Text.TaggerQL.Expression.Interpreter (
  -- * Interpreters
  Interpreter (..),
  SubInterpreter (..),
  runInterpreter,
  runSubInterpreter,

  -- * Annotators
  AnnotatedExpression (..),
  expressionAnnotation,
  AnnotatedSubExpression (..),
  subExpressionAnnotation,
  annotator,

  -- * Examples
  queryer,
  indexer,
  printer,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, local)
import Control.Monad.Trans.State.Strict (State, get, modify)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Functor.Identity (Identity (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Ix (Ix)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import Database.Tagger (
  File,
  Tag (..),
  TaggedConnection,
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelationPattern,
  queryForFileByPattern,
 )
import Text.TaggerQL.Expression.AST (
  BinaryExpression (BinaryExpression),
  BinarySubExpression (BinarySubExpression),
  Expression (..),
  ExpressionLeaf (..),
  ExpressionSubContext (ExpressionSubContext),
  FileTerm (FileTerm),
  IdentityKind,
  SubExpression (..),
  SubExpressionExtension (SubExpressionExtension),
  TagTerm (..),
  expressionIdentity,
  subExpressionIdentity,
 )
import Text.TaggerQL.Expression.Interpreter.Internal (
  joinSubTags',
  queryTags,
  toFileSet,
 )

newtype AnnotatedExpression a b = AnnotatedExpression
  {runAnnotatedExpression :: Expression ((,) a) ((,) b)}
  deriving (Show, Eq)

instance Bifunctor AnnotatedExpression where
  first :: (a -> b) -> AnnotatedExpression a c -> AnnotatedExpression b c
  first f (AnnotatedExpression expr) = AnnotatedExpression $
    case expr of
      ExpressionLeaf x0 -> ExpressionLeaf . first f $ x0
      BinaryExpressionValue (x, BinaryExpression lhs so rhs) ->
        BinaryExpressionValue
          ( f x
          , BinaryExpression
              (runAnnotatedExpression . first f . AnnotatedExpression $ lhs)
              so
              (runAnnotatedExpression . first f . AnnotatedExpression $ rhs)
          )
      ExpressionSubContextValue x0 -> ExpressionSubContextValue . first f $ x0
  second :: (b -> c) -> AnnotatedExpression a b -> AnnotatedExpression a c
  second f (AnnotatedExpression expr) =
    AnnotatedExpression $
      case expr of
        ExpressionLeaf x0 -> ExpressionLeaf x0
        BinaryExpressionValue (x, BinaryExpression lhs so rhs) ->
          BinaryExpressionValue
            ( x
            , BinaryExpression
                (runAnnotatedExpression . second f . AnnotatedExpression $ lhs)
                so
                (runAnnotatedExpression . second f . AnnotatedExpression $ rhs)
            )
        ExpressionSubContextValue (x, ExpressionSubContext tt se) ->
          ExpressionSubContextValue
            ( x
            , ExpressionSubContext
                tt
                (runAnnotatedSubExpression . fmap f . AnnotatedSubExpression $ se)
            )

newtype AnnotatedSubExpression a = AnnotatedSubExpression
  {runAnnotatedSubExpression :: SubExpression ((,) a)}
  deriving (Show, Eq)

instance Functor AnnotatedSubExpression where
  fmap :: (a -> b) -> AnnotatedSubExpression a -> AnnotatedSubExpression b
  fmap f (AnnotatedSubExpression se) = AnnotatedSubExpression $
    case se of
      SubTag x0 -> SubTag . first f $ x0
      SubBinary (x, BinarySubExpression lhs so rhs) ->
        SubBinary
          ( f x
          , BinarySubExpression
              (runAnnotatedSubExpression . fmap f . AnnotatedSubExpression $ lhs)
              so
              (runAnnotatedSubExpression . fmap f . AnnotatedSubExpression $ rhs)
          )
      SubExpression (x, SubExpressionExtension tt se') ->
        SubExpression
          ( f x
          , SubExpressionExtension
              tt
              (runAnnotatedSubExpression . fmap f . AnnotatedSubExpression $ se')
          )

{- |
 An interpreter over 'Expression` constructors.

 Provides definitions for evaluation as well as mapping a
 'SubExpression` to an 'Expression`.
-}
data Interpreter m b a = Interpreter
  { interpretSubExpression :: SubInterpreter m b
  , interpretExpressionLeaf :: ExpressionLeaf -> m a
  , interpretBinaryExpression :: SetOp -> a -> a -> m a
  , -- | An 'Expression` of the form: \"a{b}\".
    -- That computes the 'SubExpression` \"b\" after extending the computation context
    -- with \"a\".
    interpretExpressionSubContext :: TagTerm -> m b -> m a
  }

{- |
 An interpreter over 'SubExpression` constructors.

 Provides definitions for evaluation.
-}
data SubInterpreter m b = SubInterpreter
  { interpretSubTag :: TagTerm -> m b
  , -- | A 'SubExpression` with the syntax: \"a{b}\"
    -- used to extend the context that a 'SubInterpreter` is computing in.
    interpretSubExpressionExtension :: TagTerm -> m b -> m b
  , interpretBinarySubExpression :: SetOp -> b -> b -> m b
  }

{- |
 Run the given 'Interpreter` over an 'Expression` that can be mapped to its identity.
-}
runInterpreter ::
  (Monad m, IdentityKind t, IdentityKind k) =>
  Interpreter m b a ->
  Expression t k ->
  m a
runInterpreter
  itr@( Interpreter
          sitr
          iel
          ibe
          iesc
        )
  (expressionIdentity -> expr) = case expr of
    ExpressionLeaf (Identity l) -> iel l
    BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
      l <- runInterpreter itr lhs
      r <- runInterpreter itr rhs
      ibe so l r
    ExpressionSubContextValue (Identity (ExpressionSubContext tt se)) ->
      iesc tt $ runSubInterpreter sitr se

runSubInterpreter ::
  (IdentityKind k, Monad n) =>
  SubInterpreter n b ->
  SubExpression k ->
  n b
runSubInterpreter sitr@(SubInterpreter ist isee ibss) (subExpressionIdentity -> se) =
  case se of
    SubTag (Identity tt) -> ist tt
    SubBinary (Identity (BinarySubExpression lhs so rhs)) -> do
      l <- runSubInterpreter sitr lhs
      r <- runSubInterpreter sitr rhs
      ibss so l r
    SubExpression (Identity (SubExpressionExtension tt se')) ->
      isee tt $ runSubInterpreter sitr se'

{- |
 Retrieve the annotation from a given 'Expression`
-}
expressionAnnotation :: AnnotatedExpression a b -> a
expressionAnnotation (AnnotatedExpression expr) = case expr of
  ExpressionLeaf x0 -> fst x0
  BinaryExpressionValue x0 -> fst x0
  ExpressionSubContextValue x0 -> fst x0

{- |
 Retrieve the annotation from a given 'SubExpression`
-}
subExpressionAnnotation :: AnnotatedSubExpression a -> a
subExpressionAnnotation (AnnotatedSubExpression se) = case se of
  SubTag x0 -> fst x0
  SubBinary x0 -> fst x0
  SubExpression x0 -> fst x0

{- |
 Annotate the leaves of an 'Expression` with the results of an interpreter.
-}
annotator ::
  (Monad m) =>
  Interpreter m b a ->
  Interpreter m (AnnotatedSubExpression b) (AnnotatedExpression a b)
annotator (Interpreter (SubInterpreter sa sb sc) a b c) =
  Interpreter
    { interpretSubExpression =
        SubInterpreter
          { interpretSubTag = \tt ->
              fmap (AnnotatedSubExpression . SubTag . (,tt)) $
                sa tt
          , interpretBinarySubExpression =
              \so
               lhs@(subExpressionAnnotation -> lhsa)
               rhs@(subExpressionAnnotation -> rhsa) -> do
                  r <- sc so lhsa rhsa
                  return
                    ( AnnotatedSubExpression $
                        SubBinary
                          ( r
                          , BinarySubExpression
                              (runAnnotatedSubExpression lhs)
                              so
                              (runAnnotatedSubExpression rhs)
                          )
                    )
          , interpretSubExpressionExtension = \tt ase -> do
              r <- sb tt . fmap subExpressionAnnotation $ ase
              aser <- fmap runAnnotatedSubExpression ase
              return
                ( AnnotatedSubExpression $
                    SubExpression
                      (r, SubExpressionExtension tt aser)
                )
          }
    , interpretBinaryExpression =
        \so
         lhs@(expressionAnnotation -> lhsa)
         rhs@(expressionAnnotation -> rhsa) -> do
            r <- b so lhsa rhsa
            return . AnnotatedExpression . BinaryExpressionValue $
              ( r
              , BinaryExpression
                  (runAnnotatedExpression lhs)
                  so
                  (runAnnotatedExpression rhs)
              )
    , interpretExpressionLeaf = \leaf ->
        fmap (AnnotatedExpression . ExpressionLeaf . (,leaf)) $ a leaf
    , interpretExpressionSubContext = \tt ase -> do
        aser <- fmap runAnnotatedSubExpression ase
        r <- c tt . fmap subExpressionAnnotation $ ase
        return . AnnotatedExpression . ExpressionSubContextValue $
          (r, ExpressionSubContext tt aser)
    }

{- |
 Query a database for a set of files.

 The heart and soul of this program.
-}
queryer ::
  Interpreter
    (ReaderT (HashSet Tag) (ReaderT TaggedConnection IO))
    (HashSet Tag)
    (HashSet File)
queryer =
  Interpreter
    { interpretSubExpression =
        SubInterpreter
          { interpretSubTag = \tt -> do
              c <- lift ask
              supertags <- ask
              subtags <- liftIO . fmap (HS.fromList . map tagSubtagOfId) $ queryTags tt c
              return $ joinSubTags' supertags subtags
          , interpretBinarySubExpression = \so lhs rhs -> do
              pure $
                ( case so of
                    Union -> HS.union
                    Intersect -> HS.intersection
                    Difference -> HS.difference
                )
                  lhs
                  rhs
          , interpretSubExpressionExtension = \tt se -> do
              c <- lift ask
              supertags <- ask
              nextTagEnv <- liftIO . fmap HS.fromList $ queryTags tt c
              subExprResult <- HS.map tagSubtagOfId <$> local (const nextTagEnv) se
              return $ joinSubTags' supertags subExprResult
          }
    , interpretExpressionLeaf = \leaf -> case leaf of
        FileTermValue (FileTerm t) ->
          lift ask
            >>= liftIO . fmap HS.fromList . queryForFileByPattern t
        TagTermValue tt ->
          lift ask
            >>= liftIO . fmap HS.fromList
              . ( case tt of
                    DescriptorTerm txt -> flatQueryForFileByTagDescriptorPattern txt
                    MetaDescriptorTerm txt -> flatQueryForFileOnMetaRelationPattern txt
                )
    , interpretBinaryExpression = \so lhs rhs ->
        pure $
          ( case so of
              Union -> HS.union
              Intersect -> HS.intersection
              Difference -> HS.difference
          )
            lhs
            rhs
    , interpretExpressionSubContext = \tt se -> do
        c <- lift ask
        supertags <- liftIO . fmap HS.fromList $ queryTags tt c
        result <- local (const supertags) se
        liftIO . flip toFileSet c $ result
    }

{- |
 Count the total number of expressions and subExpressions.
-}
counter ::
  (Ix a, Num a) =>
  Interpreter (State a) a a
counter =
  Interpreter
    { interpretSubExpression =
        SubInterpreter
          { interpretBinarySubExpression = \_ _ _ -> get <* modify (1 +)
          , interpretSubExpressionExtension = \_ se -> get <* modify (1 +) <* se
          , interpretSubTag = \_ -> get <* modify (1 +)
          }
    , interpretExpressionLeaf = \_ -> get <* modify (1 +)
    , interpretBinaryExpression = \_ _ _ -> get <* modify (1 +)
    , interpretExpressionSubContext = \_ sc -> get <* modify (1 +) <* sc
    }

{- |
 Supply an index to each 'Expression` or 'SubExpression` in evaluation order.
-}
indexer ::
  (Ix a, Num a) =>
  Interpreter
    (State a)
    (AnnotatedSubExpression a)
    (AnnotatedExpression a a)
indexer =
  annotator
    counter

{- |
 Pretty print an 'Expression`.

 Tracks state in the snd term, can be discarded after interpretatioon.
-}
printer :: Interpreter Identity (Text, Bool) (Text, Bool)
printer =
  Interpreter
    { interpretSubExpression =
        SubInterpreter
          { interpretBinarySubExpression = \so (lhs, _) (rhs, n) ->
              Identity . (,True) $
                lhs
                  <> ( case so of
                        Union -> " | "
                        Intersect -> " "
                        Difference -> " ! "
                     )
                  <> (if n then (\t -> "(" <> t <> ")") else id) rhs
          , interpretSubExpressionExtension = \tt (Identity (se, _)) ->
              Identity . (,False) $ formatTT tt <> " {" <> se <> "}"
          , interpretSubTag = Identity . (,False) . formatTT
          }
    , interpretBinaryExpression = \so (lhs, _) (rhs, n) ->
        Identity . (,True) $
          lhs
            <> ( case so of
                  Union -> " | "
                  Intersect -> " "
                  Difference -> " ! "
               )
            <> (if n then (\t -> "(" <> t <> ")") else id) rhs
    , interpretExpressionLeaf = \leaf -> Identity . (,False) $ case leaf of
        FileTermValue (FileTerm t) -> "p." <> t
        TagTermValue tt -> formatTT tt
    , interpretExpressionSubContext = \tt (Identity (se, _)) ->
        Identity . (,False) $
          formatTT tt <> " {" <> se <> "}"
    }
 where
  formatTT tt = case tt of
    DescriptorTerm t -> "d." <> t
    MetaDescriptorTerm t -> t
