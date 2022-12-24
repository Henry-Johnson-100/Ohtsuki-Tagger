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
  Interpreter (..),
  runInterpreter,
  SubInterpreter (..),
  queryer,
) where

-- AnnotatedExpression (..),
-- annotation,
-- Interpreter (..),
-- runInterpreter,
-- annotate,

-- -- * Manipulation
-- withInterpreterContext,
-- fanoutInterpreter,
-- mapInterpreter,
-- mapMInterpreter,

-- -- * Interpreters
-- queryer,
-- prettyPrinter,
-- voider,
-- counter,
-- hasher,

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.State.Strict (State, get, modify)
import Data.Functor.Identity (Identity (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
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
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Interpreter.Internal (
  evalSubExpression,
  joinSubTags',
  queryTags,
  toFileSet,
 )

data SubInterpreter n b = SubInterpreter
  { interpretSubTag :: TagTerm -> n b
  , interpretSubExpressionExtension :: TagTerm -> n b -> n b
  , interpretBinarySubExpression :: SetOp -> b -> b -> n b
  }

data Interpreter m n b a = Interpreter
  { interpretSubExpression :: SubInterpreter n b
  , interpretExpressionLeaf :: ExpressionLeaf -> m a
  , interpretBinaryExpression :: SetOp -> a -> a -> m a
  , interpretExpressionSubContext :: TagTerm -> n b -> m a
  }

runInterpreter ::
  (Monad m, Monad n, IdentityKind t, IdentityKind k) =>
  Interpreter m n b a ->
  Expression t k ->
  m a
runInterpreter
  itr@( Interpreter
          ( SubInterpreter
              ist
              isee
              ibss
            )
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
      iesc
        tt
        $ subItr se
   where
    subItr se =
      case se of
        SubTag (Identity tt) -> ist tt
        SubBinary (Identity (BinarySubExpression lhs so rhs)) -> do
          l <- subItr lhs
          r <- subItr rhs
          ibss so l r
        SubExpression (Identity (SubExpressionExtension tt se')) -> isee tt $ subItr se'

queryer ::
  Interpreter
    (ReaderT TaggedConnection IO)
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
          ask
            >>= liftIO . fmap HS.fromList . queryForFileByPattern t
        TagTermValue tt ->
          ask
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
        c <- ask
        supertags <- liftIO . fmap HS.fromList $ queryTags tt c
        result <- runReaderT se supertags
        liftIO . flip toFileSet c $ result
    }

-- {- |
--  A data type containing behavior for how to interpret an 'Expression`.

--  Where 'm` is the context to interpret in, typically some monad, and
--  t is the final output of the interpreter.
-- -}
-- data Interpreter m t = Interpreter
--   { interpretBinaryOperation :: SetOp -> t -> t -> m t
--   , interpretExpressionLeaf :: ExpressionLeaf -> m t
--   }

-- {- |
--  Given two 'Interpreter` that run in the same context, combine their output.
-- -}
-- fanoutInterpreter ::
--   Applicative f =>
--   Interpreter f a ->
--   Interpreter f b ->
--   Interpreter f (a, b)
-- fanoutInterpreter (Interpreter aob ael) (Interpreter bob bel) =
--   Interpreter
--     { interpretBinaryOperation = \so (alhs, blhs) (arhs, brhs) ->
--         (,) <$> aob so alhs arhs <*> bob so blhs brhs
--     , interpretExpressionLeaf = \leaf -> (,) <$> ael leaf <*> bel leaf
--     }

-- {- |
--  Map an 'Interpreter` from one context to another.
-- -}
-- withInterpreterContext :: (m a -> n a) -> Interpreter m a -> Interpreter n a
-- withInterpreterContext f (Interpreter iob iel) =
--   Interpreter
--     { interpretBinaryOperation = \so lhs rhs -> f $ iob so lhs rhs
--     , interpretExpressionLeaf = f . iel
--     }

-- {- |
--  Map an 'Interpreter` over isomorphic types.

--  An 'Interpreter` cannot be a simple Functor, but given an isomorphism it can convert to
--  and from an intermediate type during interpretation.
-- -}
-- mapInterpreter :: Functor m => (a -> b) -> (b -> a) -> Interpreter m a -> Interpreter m b
-- mapInterpreter to from (Interpreter iob iel) =
--   Interpreter
--     { interpretBinaryOperation = \so lhs rhs -> fmap to $ iob so (from lhs) (from rhs)
--     , interpretExpressionLeaf = fmap to . iel
--     }

-- {- |
--  Map an 'Interpreter` over types that are monadically isomorphic.

--  More flexible than 'mapInterpreter` though a bit more work. With this function it is
--  possible to declare a kind of isomorphism that exists during runtime. Using
--  the type m to be some MonadIO and the isomorphisms as some kind of storage and retrieval
--  of the appropriate values.
-- -}
-- mapMInterpreter ::
--   Monad m =>
--   (a -> m c) ->
--   (c -> m a) ->
--   Interpreter m a ->
--   Interpreter m c
-- mapMInterpreter toM fromM (Interpreter iob iel) =
--   Interpreter
--     { interpretBinaryOperation = \so lhs rhs -> do
--         lhsM <- fromM lhs
--         rhsM <- fromM rhs
--         iob so lhsM rhsM >>= toM
--     , interpretExpressionLeaf = toM <=< iel
--     }

-- {- |
--  newtype wrapper for an 'Expression ((,) a)`.

--  denoting some auxiliary information at each leaf in the 'Expression`
-- -}
-- newtype AnnotatedExpression a = AnnotatedExpression {runAnnotation :: Expression ((,) a)}
--   deriving (Show, Eq)

-- instance Functor AnnotatedExpression where
--   fmap :: (a -> b) -> AnnotatedExpression a -> AnnotatedExpression b
--   fmap f (AnnotatedExpression expr) = AnnotatedExpression $ case expr of
--     ExpressionLeaf (x, i) -> ExpressionLeaf (f x, i)
--     BinaryExpressionValue
--       ( x
--         , BinaryExpression
--             lhs
--             so
--             rhs
--         ) ->
--         BinaryExpressionValue
--           ( f x
--           , BinaryExpression
--               (runAnnotation . fmap f . AnnotatedExpression $ lhs)
--               so
--               (runAnnotation . fmap f . AnnotatedExpression $ rhs)
--           )

-- {- |
--  Return the given 'Expression` annotation.
-- -}
-- annotation :: AnnotatedExpression a -> a
-- annotation (AnnotatedExpression expr) = case expr of
--   ExpressionLeaf x0 -> fst x0
--   BinaryExpressionValue x0 -> fst x0

-- {- |
--  Interpret the given 'Expression` as if it were @Expression Identity@.
-- -}
-- runInterpreter ::
--   (Monad m, ExpressionIdentity l) =>
--   Interpreter m t ->
--   Expression l ->
--   m t
-- runInterpreter (Interpreter ibo iel) = interpret ibo iel . expressionIdentity
--  where
--   interpret ::
--     Monad m =>
--     (SetOp -> t -> t -> m t) ->
--     (ExpressionLeaf -> m t) ->
--     Expression Identity ->
--     m t
--   interpret
--     dispatchComb
--     onLeaf
--     expr =
--       case expr of
--         ExpressionLeaf (Identity l) -> onLeaf l
--         BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
--           lhsI <- interpret' lhs
--           rhsI <- interpret' rhs
--           dispatchComb so lhsI rhsI
--      where
--       interpret' =
--         interpret
--           dispatchComb
--           onLeaf

-- {- |
--  Given any 'Interpreter` i and any 'Expression` e, then the following should (hopefully)
--  be true:

--  >interpret i e = fmap annotation (annotate i e)
-- -}
-- annotate ::
--   (Monad m, ExpressionIdentity l) =>
--   Interpreter m t ->
--   Expression l ->
--   m (AnnotatedExpression t)
-- annotate itr@(Interpreter ibo iel) expr =
--   case expressionIdentity expr of
--     ExpressionLeaf (Identity leaf) ->
--       AnnotatedExpression . ExpressionLeaf . (,leaf) <$> iel leaf
--     BinaryExpressionValue (Identity (BinaryExpression lhs so rhs)) -> do
--       lhsI <- annotate itr lhs
--       rhsI <- annotate itr rhs
--       combination <- ibo so (annotation lhsI) (annotation rhsI)
--       return . AnnotatedExpression . BinaryExpressionValue $
--         (combination, BinaryExpression (runAnnotation lhsI) so (runAnnotation rhsI))

-- {- |
--  The 'Interpreter` that governs querying a database for a set of 'File`s.

--  The very heart and soul of this program.
-- -}
-- queryer :: Interpreter (ReaderT TaggedConnection IO) (HashSet File)
-- queryer =
--   Interpreter
--     { interpretBinaryOperation =
--         \so lhs rhs ->
--           pure $
--             ( case so of
--                 Union -> HS.union
--                 Intersect -> HS.intersection
--                 Difference -> HS.difference
--             )
--               lhs
--               rhs
--     , interpretExpressionLeaf = \leaf -> case leaf of
--         FileTermValue (FileTerm t) ->
--           ask
--             >>= liftIO . fmap HS.fromList . queryForFileByPattern t
--         TagTermValue tt ->
--           ask
--             >>= liftIO . fmap HS.fromList
--               . ( case tt of
--                     DescriptorTerm txt -> flatQueryForFileByTagDescriptorPattern txt
--                     MetaDescriptorTerm txt -> flatQueryForFileOnMetaRelationPattern txt
--                 )
--         TagExpressionValue tt se -> do
--           c <- ask
--           supertags <- liftIO . fmap HS.fromList $ queryTags tt c
--           subExprResult <- evalSubExpression se supertags
--           liftIO $ toFileSet subExprResult c
--     }

-- {- |
--  The return type of this Interpreter is a tuple with boolean state to keep track
--  of nested binary operations to correctly apply parentheses when printing.

--  it can be discarded after interpretation.
-- -}
-- prettyPrinter :: Interpreter Identity (Text, Bool)
-- prettyPrinter =
--   Interpreter
--     { interpretBinaryOperation = \so (lhs, _) (rhs, rIsNested) ->
--         Identity . (,True) $
--           (lhs <> formatSO so <> (if rIsNested then (\t -> "(" <> t <> ")") else id) rhs)
--     , interpretExpressionLeaf = \leaf ->
--         Identity . (,False) $
--           ( case leaf of
--               FileTermValue (FileTerm t) -> "p." <> t
--               TagTermValue tt -> formatTT tt
--               TagExpressionValue tt se -> formatTT tt <> " {" <> formatSe se <> "}"
--           )
--     }
--  where
--   formatTT tt = case tt of
--     DescriptorTerm txt -> "d." <> txt
--     MetaDescriptorTerm txt -> txt
--   formatSO so = case so of
--     Union -> " | "
--     Intersect -> " & "
--     Difference -> " ! "
--   formatSe se =
--     case se of
--       SubBinary lhs so rhs ->
--         formatSe lhs <> formatSO so
--           <> (if isNestedSubExpr rhs then "(" <> formatSe rhs <> ")" else formatSe rhs)
--       SubTag tt -> formatTT tt
--       SubExpression tt se' -> formatTT tt <> " {" <> formatSe se' <> "}"
--   isNestedSubExpr se =
--     case se of
--       SubBinary{} -> True
--       _notNested -> False

-- {- |
--  Voids an expression.

--  Used to easily create a spine-like functor over an 'Expression`

--  @
--  annotate voider ::
--   ExpressionIdentity l => Expression l -> Identity (AnnotatedExpression ())
--  @
-- -}
-- voider :: Interpreter Identity ()
-- voider =
--   Interpreter
--     { interpretBinaryOperation = \_ _ _ -> pure ()
--     , interpretExpressionLeaf = \_ -> pure ()
--     }

-- {- |
--  Counts the leafs of an 'Expression` by order of evaluation.

--  Annotating with this 'Interpreter` creates an 'Expression` where each leaf has an index.
-- -}
-- counter :: (Ix a, Num a) => Interpreter (State a) a
-- counter =
--   Interpreter
--     { interpretBinaryOperation = \_ _ _ -> get <* modify (+ 1)
--     , interpretExpressionLeaf = \_ -> get <* modify (+ 1)
--     }

-- {- |
--  Hashes an expression based on the formatted text of its leaves.
-- -}
-- hasher :: Interpreter Identity Int
-- hasher =
--   let (Interpreter _ iel) = prettyPrinter
--    in Interpreter
--         { interpretBinaryOperation = \so lhs rhs ->
--             Identity $
--               hashWithSalt
--                 (fromEnum so)
--                 (hashWithSalt lhs rhs)
--         , interpretExpressionLeaf = fmap (hash . fst) . iel
--         }