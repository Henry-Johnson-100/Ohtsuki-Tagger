{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- for this will just be a testing ground for traversing parsers, not just the
-- type declarations

module Event.Parser.Type () where

import qualified Data.Text as T
import Database.Tagger.Access
import Database.Tagger.Type
import Text.Parsec
import Type.Model.Prim

type QueryParser a = Parsec T.Text AppQueryInformation a

-- throw these in Database.Tagger.Type when done

newtype FilePattern = FilePattern T.Text deriving (Show, Eq)

newtype DescriptorPattern = DescriptorPattern T.Text deriving (Show, Eq)

--

newtype SubjectPhrase a = SubjectPhrase {phraseContents :: a}
  deriving (Show, Eq, Functor, Foldable)

data AppQueryInformation = AppQueryInformation
  { aqiSetArithmetic :: !FileSetArithmetic,
    aqiQueryCriteria :: !QueryCriteria
  }
  deriving (Show, Eq)

data QueryToken a = QueryToken
  { tokenCriteria :: !QueryCriteria,
    tokenContents :: !a
  }
  deriving (Show, Eq, Functor, Foldable)

data QueryClause a = QueryClause
  { clauseArithmetic :: !FileSetArithmetic,
    clauseContents :: !a
  }
  deriving (Show, Eq, Functor, Foldable)
