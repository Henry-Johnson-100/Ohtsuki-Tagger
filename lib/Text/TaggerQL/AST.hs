{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Text.TaggerQL.AST
Description : Defines the components of a TaggerQL AST.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.AST (
  Request (..),
  Sentence (..),
  Term (..),
  formatRequest,
) where

import qualified Data.List as L
import Data.Tagger (QueryCriteria (..), SetOp (..))

{- |
 A collection of 'Sentence`s.

 A 'Request` is a whole TaggerQL query.
-}
newtype Request a = Request [Sentence a] deriving (Show, Eq, Functor)

{- |
 A collection of 'Term`s.
-}
newtype Sentence a = Sentence [Term a] deriving (Show, Eq, Functor)

{- |
 The smallest valid unit of a TaggerQL query.

 It may have a predicate in addition to its basis if the term is a subquery.

 Most complex searches can be desugared into a list of terms.
-}
data Term a = Term
  { -- | How to combine this 'Term` with other results.
    termSetOperation :: SetOp
  , -- | How to search with this 'Term`
    termCriteria :: QueryCriteria
  , -- | The pattern with which to search.
    termBasis :: a
  , -- | The predicate that modifies the search, such as searching for subtags.
    termPredicate :: [Term a]
  }
  deriving (Show, Eq, Functor)

{- |
 Pretty-print a 'Request`
-}
formatRequest :: Show a => Request a -> String
formatRequest (Request sentences) =
  "Request\n" ++ (L.intercalate "\n" . map (formatSentence 1) $ sentences)

formatSentence :: Show a => Int -> Sentence a -> String
formatSentence indentLevel (Sentence terms) =
  concat (replicate indentLevel "  ")
    ++ "Sentence\n"
    ++ (L.intercalate "\n" . map (formatTerm (indentLevel + 1)) $ terms)

formatTerm :: Show a => Int -> Term a -> String
formatTerm indentLevel (Term so qc b ps) =
  concat (replicate indentLevel "  ")
    ++ formatSetOp so
    ++ formatCriteria qc
    ++ show b
    ++ if null ps
      then ""
      else
        "\n"
          ++ (L.intercalate "\n" . map (formatTerm (indentLevel + 1)) $ ps)

formatCriteria :: QueryCriteria -> String
formatCriteria qc =
  case qc of
    DescriptorCriteria -> "D."
    MetaDescriptorCriteria -> "R."
    FilePatternCriteria -> "P."
    UntaggedCriteria -> "U."

formatSetOp :: SetOp -> String
formatSetOp so =
  case so of
    Union -> "U| "
    Intersect -> "I| "
    Difference -> "D| "