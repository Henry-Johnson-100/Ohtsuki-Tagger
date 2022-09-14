{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.QueryBuilder.AST.Core () where

import Class
import Control.Lens hiding (from, to)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Tagger
import Data.Text (Text)
import Text.TaggerQL.AST

data QueryBuilderComplexTerm
  = QueryBuilderTerm :~<- (Seq QueryBuilderComplexTerm)
  | QBBottom (Term Text)
  deriving (Show, Eq)

-- I believe this instance actually is not a technically correct isomorphism
-- Because an empty complex term is converted to bottom, even though a complex term
-- has a NonEmpty, because a Sequence can be empty, it is not technically isomorphic.
instance Isomorphic QueryBuilderComplexTerm (ComplexTerm Text) where
  to :: QueryBuilderComplexTerm -> ComplexTerm Text
  to qb =
    case qb of
      (QBTerm t) :~<- s ->
        case F.toList s of
          [] -> Bottom t
          xs -> t :<- (NE.fromList . map to $ xs)
      QBBottom t -> Bottom t
  from :: ComplexTerm Text -> QueryBuilderComplexTerm
  from ct =
    case ct of
      Bottom t -> QBBottom t
      t :<- cts -> from t :~<- (fmap from . Seq.fromList . F.toList $ cts)

newtype QueryBuilderSimpleTerm = QBSimpleTerm (SimpleTerm Text)
  deriving (Show, Eq)

instance Isomorphic QueryBuilderSimpleTerm (SimpleTerm Text) where
  to :: QueryBuilderSimpleTerm -> SimpleTerm Text
  to (QBSimpleTerm st) = st
  from :: SimpleTerm Text -> QueryBuilderSimpleTerm
  from = QBSimpleTerm

qbSimpleTerm :: Lens' QueryBuilderSimpleTerm (SimpleTerm Text)
qbSimpleTerm =
  lens
    to
    (\_ t -> from t)

newtype QueryBuilderTerm = QBTerm (Term Text)
  deriving (Show, Eq)

instance Isomorphic QueryBuilderTerm (Term Text) where
  to :: QueryBuilderTerm -> Term Text
  to (QBTerm t) = t
  from :: Term Text -> QueryBuilderTerm
  from = QBTerm

qbTermCriteria :: Lens' QueryBuilderTerm QueryCriteria
qbTermCriteria =
  lens
    ((termCriteria :: Term Text -> QueryCriteria) . to)
    (\(QBTerm t) qc -> from (t{termCriteria = qc}))

qbTermPattern :: Lens' QueryBuilderTerm Text
qbTermPattern =
  lens
    (termPattern . to)
    (\(QBTerm t) tp -> from (t{termPattern = tp}))