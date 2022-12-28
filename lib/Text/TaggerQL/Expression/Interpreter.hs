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
  tagger,
  indexer,
  printer,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, local)
import Control.Monad.Trans.State.Strict (State, get, modify)
import Data.Functor.Identity (Identity (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Ix (Ix)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import Database.Tagger (
  Descriptor (..),
  File,
  RecordKey (..),
  Tag (..),
  TaggedConnection,
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelationPattern,
  insertTags,
  queryForDescriptorByPattern,
  queryForFileByPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,
 )
import Text.TaggerQL.Expression.AST (
  BinaryExpression (..),
  Expression (..),
  ExpressionLeaf (..),
  FileTerm (FileTerm),
  IdentityKind,
  SubExpression (..),
  TagTerm (..),
  TagTermExtension (..),
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

newtype AnnotatedSubExpression a = AnnotatedSubExpression
  {runAnnotatedSubExpression :: SubExpression ((,) a)}
  deriving (Show, Eq)

{- |
 An interpreter over 'Expression` constructors.

 Provides definitions for evaluation as well as mapping a
 'SubExpression` to an 'Expression`.
-}
data Interpreter m b a = Interpreter
  { interpretSubExpression :: SubInterpreter m b
  , interpretExpressionLeaf :: ExpressionLeaf -> m a
  , interpretBinaryExpression :: BinaryExpression a -> m a
  , -- | An 'Expression` of the form: \"a{b}\".
    -- That computes the 'SubExpression` \"b\" after extending the computation context
    -- with \"a\".
    interpretExpressionSubContext :: TagTermExtension (m b) -> m a
  }

{- |
 An interpreter over 'SubExpression` constructors.

 Provides definitions for evaluation.
-}
data SubInterpreter m b = SubInterpreter
  { interpretSubTag :: TagTerm -> m b
  , -- | A 'SubExpression` with the syntax: \"a{b}\"
    -- used to extend the context that a 'SubInterpreter` is computing in.
    interpretSubExpressionExtension :: TagTermExtension (m b) -> m b
  , interpretBinarySubExpression :: BinaryExpression b -> m b
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
      ibe $ BinaryExpression l so r
    ExpressionTagTermExtension (Identity (TagTermExtension tt se)) -> do
      iesc . TagTermExtension tt . runSubInterpreter sitr $ se

runSubInterpreter ::
  (IdentityKind k, Monad n) =>
  SubInterpreter n b ->
  SubExpression k ->
  n b
runSubInterpreter sitr@(SubInterpreter ist isee ibss) (subExpressionIdentity -> se) =
  case se of
    SubTag (Identity tt) -> ist tt
    SubBinary (Identity (BinaryExpression lhs so rhs)) -> do
      l <- runSubInterpreter sitr lhs
      r <- runSubInterpreter sitr rhs
      ibss $ BinaryExpression l so r
    SubExpression (Identity tte) -> isee . fmap (runSubInterpreter sitr) $ tte

{- |
 Retrieve the annotation from a given 'Expression`
-}
expressionAnnotation :: AnnotatedExpression a b -> a
expressionAnnotation (AnnotatedExpression expr) = case expr of
  ExpressionLeaf x0 -> fst x0
  BinaryExpressionValue x0 -> fst x0
  ExpressionTagTermExtension x0 -> fst x0

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
          , interpretBinarySubExpression = \bin@(BinaryExpression lhs so rhs) -> do
              r <- sc . fmap subExpressionAnnotation $ bin
              return . AnnotatedSubExpression . SubBinary $
                ( r
                , BinaryExpression
                    (runAnnotatedSubExpression lhs)
                    so
                    (runAnnotatedSubExpression rhs)
                )
          , interpretSubExpressionExtension = \tte@(TagTermExtension tt se) -> do
              r <- sb . fmap (fmap subExpressionAnnotation) $ tte
              r' <- fmap runAnnotatedSubExpression se
              return . AnnotatedSubExpression . SubExpression $
                (r, TagTermExtension tt r')
          }
    , interpretBinaryExpression =
        \bin@(BinaryExpression lhs so rhs) -> do
          r <- b . fmap expressionAnnotation $ bin
          return . AnnotatedExpression . BinaryExpressionValue $
            ( r
            , BinaryExpression
                (runAnnotatedExpression lhs)
                so
                (runAnnotatedExpression rhs)
            )
    , interpretExpressionLeaf = \leaf ->
        fmap (AnnotatedExpression . ExpressionLeaf . (,leaf)) $ a leaf
    , interpretExpressionSubContext = \tte@(TagTermExtension tt se) -> do
        r <- c . fmap (fmap subExpressionAnnotation) $ tte
        r' <- fmap runAnnotatedSubExpression se
        return . AnnotatedExpression
          . ExpressionTagTermExtension
          $ (r, TagTermExtension tt r')
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
          , interpretBinarySubExpression = \(BinaryExpression lhs so rhs) -> do
              pure $
                ( case so of
                    Union -> HS.union
                    Intersect -> HS.intersection
                    Difference -> HS.difference
                )
                  lhs
                  rhs
          , interpretSubExpressionExtension = \(TagTermExtension tt se) -> do
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
    , interpretBinaryExpression = \(BinaryExpression lhs so rhs) ->
        pure $
          ( case so of
              Union -> HS.union
              Intersect -> HS.intersection
              Difference -> HS.difference
          )
            lhs
            rhs
    , interpretExpressionSubContext = \(TagTermExtension tt se) -> do
        c <- lift ask
        supertags <- liftIO . fmap HS.fromList $ queryTags tt c
        result <- local (const supertags) se
        liftIO . flip toFileSet c $ result
    }

{- |
 A 'SubInterpreter` that tags a given file with the tags defined by the given
 'SubExpression`
 as subtags of the given list of 'Tag` Id's if present.
-}
tagger ::
  SubInterpreter
    ( ReaderT (RecordKey File, Maybe [RecordKey Tag], TaggedConnection) IO
    )
    [RecordKey Tag]
tagger =
  SubInterpreter
    { -- Tags inserted by a BinaryExpression are indeterminate and empty by default.
      interpretBinarySubExpression = const . pure $ []
    , interpretSubExpressionExtension =
        \( TagTermExtension
            (SubTag . Identity -> st)
            se
          ) -> do
            insertedSubtags <- runSubInterpreter tagger st
            local (\(x, _, z) -> (x, Just insertedSubtags, z)) se
    , interpretSubTag = \tt -> do
        (fk, supertags, c) <- ask
        let txt = termTxt tt
        withDescriptors <- qDescriptor txt
        let tagTriples =
              (fk,,) <$> (descriptorId <$> withDescriptors)
                <*> maybe [Nothing] (fmap Just) supertags
        void . liftIO . insertTags tagTriples $ c
        -- Tag insertion may fail because some tags of the same form already exist.
        -- This query gets all of those pre-existing tags,
        -- and returns them as if they were just made.
        case supertags of
          -- If nothing, then these are top-level tags
          Nothing ->
            map tagId
              <$> liftIO
                ( queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf
                    fk
                    txt
                    c
                )
          -- If just, these are subtags of existing tags.
          Just _ ->
            map tagId . unions
              <$> liftIO
                ( mapM
                    (`queryForTagBySubTagTriple` c)
                    (third fromJust <$> tagTriples)
                )
    }
 where
  termTxt tt =
    case tt of
      DescriptorTerm txt -> txt
      MetaDescriptorTerm txt -> txt
  qDescriptor txt = asks (\(_, _, c) -> c) >>= liftIO . queryForDescriptorByPattern txt
  unions :: (Foldable t, Eq a) => t [a] -> [a]
  unions xs = if null xs then [] else L.foldl' L.union [] xs
  third f (x, y, z) = (x, y, f z)

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
          { interpretBinarySubExpression = \_ -> get <* modify (1 +)
          , interpretSubExpressionExtension = \(TagTermExtension _ se) ->
              get <* modify (1 +) <* se
          , interpretSubTag = \_ -> get <* modify (1 +)
          }
    , interpretExpressionLeaf = \_ -> get <* modify (1 +)
    , interpretBinaryExpression = \_ -> get <* modify (1 +)
    , interpretExpressionSubContext = \(TagTermExtension _ sc) ->
        get <* modify (1 +) <* sc
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
          { interpretBinarySubExpression = \(BinaryExpression (lhs, _) so (rhs, n)) ->
              Identity . (,True) $
                lhs
                  <> ( case so of
                        Union -> " | "
                        Intersect -> " "
                        Difference -> " ! "
                     )
                  <> (if n then (\t -> "(" <> t <> ")") else id) rhs
          , interpretSubExpressionExtension = \(TagTermExtension tt (Identity (se, _))) ->
              Identity . (,False) $ formatTT tt <> " {" <> se <> "}"
          , interpretSubTag = Identity . (,False) . formatTT
          }
    , interpretBinaryExpression = \(BinaryExpression (lhs, _) so (rhs, n)) ->
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
    , interpretExpressionSubContext = \(TagTermExtension tt (Identity (se, _))) ->
        Identity
          . (,False)
          $ formatTT tt <> " {" <> se <> "}"
    }
 where
  formatTT tt = case tt of
    DescriptorTerm t -> "d." <> t
    MetaDescriptorTerm t -> t
