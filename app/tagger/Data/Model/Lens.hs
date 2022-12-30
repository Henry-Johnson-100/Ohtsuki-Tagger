{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant return" #-}

module Data.Model.Lens (
  module Data.Model.Lens,
) where

import Control.Lens (
  Lens',
  abbreviatedFields,
  lens,
  makeLensesWith,
 )
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (
  State,
  evalState,
  get,
  modify,
  runState,
 )
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (isLeft, isRight)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe, isNothing)
import Data.Model.Core (
  DescriptorInfo,
  DescriptorTreeModel,
  FileInfo,
  FileSelectionModel,
  FileSelectionTagListModel,
  FocusedFileModel,
  PositioningModel,
  QueryModel,
  TaggerInfoModel,
  TaggerModel,
  createDescriptorInfo,
  createFileInfo,
 )
import Text.TaggerQL.Expression.AST (
  Expression (..),
  SubExpression (..),
  TagTermExtension (TagTermExtension),
 )
import Text.TaggerQL.Expression.Engine (
  ExpressionInterpreter (..),
  SubExpressionInterpreter (
    SubExpressionInterpreter,
    interpretBinarySubExpression,
    interpretSubExpression,
    interpretSubTag
  ),
  runExpressionInterpreter,
 )

newtype TaggerLens a b = TaggerLens {taggerLens :: Lens' a b}

instance Show (TaggerLens a b) where
  show :: TaggerLens a b -> String
  show _ = "Tagger Lens'"

instance Eq (TaggerLens a b) where
  (==) :: TaggerLens a b -> TaggerLens a b -> Bool
  _ == _ = True

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''FocusedFileModel

makeLensesWith abbreviatedFields ''DescriptorInfo

makeLensesWith abbreviatedFields ''FileSelectionModel

makeLensesWith abbreviatedFields ''FileSelectionTagListModel

makeLensesWith abbreviatedFields ''FileInfo

makeLensesWith abbreviatedFields ''TaggerInfoModel

makeLensesWith abbreviatedFields ''PositioningModel

makeLensesWith abbreviatedFields ''QueryModel

{- |
 A lens over indices of an 'Expression`. Where each element is either an
 'Expression` or 'SubExpression`.

 An 'Expression` is 1-indexed in order of evaluation.
-}
expressionIx :: Int -> Lens' Expression (Maybe (Either SubExpression Expression))
expressionIx ix =
  lens
    ( fst
        . snd
        . flip runState (Nothing, 1)
        . runExpressionInterpreter getter
    )
    ( \expr mReplace ->
        maybe
          expr
          ( \replace ->
              flip evalState 1 $
                runExpressionInterpreter (setter replace) expr
          )
          mReplace
    )
 where
  setter ::
    Either SubExpression Expression ->
    ExpressionInterpreter (State Int) SubExpression Expression
  setter eexpr =
    ExpressionInterpreter
      { subExpressionInterpreter =
          SubExpressionInterpreter
            { interpretSubTag = withLeftEx . SubTag
            , interpretBinarySubExpression = withLeftEx . BinarySubExpression
            , interpretSubExpression = \(TagTermExtension tt se) ->
                se >>= withLeftEx . SubExpression . TagTermExtension tt
            }
      , interpretFileTerm = withRightEx . FileTermValue
      , interpretTagTerm = withRightEx . TagTermValue
      , interpretBinaryExpression = withRightEx . BinaryExpression
      , interpretTagExpression = \(TagTermExtension tt se) ->
          se >>= withRightEx . TagExpression . TagTermExtension tt
      }
   where
    withLeftEx = withExCond isLeft (\(Left x) -> x)
    withRightEx = withExCond isRight (\(Right x) -> x)
    withExCond cond tV eV = do
      n <- get
      let r = if n == ix && cond eexpr then tV eexpr else eV
      modify (1 +)
      return r

  getter ::
    ExpressionInterpreter
      (State (Maybe (Either SubExpression Expression), Int))
      SubExpression
      Expression
  getter =
    ExpressionInterpreter
      { subExpressionInterpreter =
          SubExpressionInterpreter
            { interpretSubTag = withEx Left . SubTag
            , interpretBinarySubExpression = withEx Left . BinarySubExpression
            , interpretSubExpression = \(TagTermExtension tt se) ->
                se >>= withEx Left . SubExpression . TagTermExtension tt
            }
      , interpretFileTerm = withEx Right . FileTermValue
      , interpretTagTerm = withEx Right . TagTermValue
      , interpretBinaryExpression = withEx Right . BinaryExpression
      , interpretTagExpression = \(TagTermExtension tt se) -> do
          se >>= withEx Right . TagExpression . TagTermExtension tt
      }
   where
    withEx e ex = do
      (r, n) <- get
      when (n == ix && isNothing r) . modify . first . const . Just . e $ ex
      modify . second $ (1 +)
      return ex

{-# INLINE fileInfoAt #-}

{- |
 Derived lens for retrieving a 'FileInfo` from an IntMap at the given key.

 If a FileInfo does not exist there, return a default FileInfo
-}
fileInfoAt :: Int -> Lens' (IntMap FileInfo) FileInfo
fileInfoAt n =
  lens
    (fromMaybe createFileInfo . IntMap.lookup n)
    (flip (IntMap.insert n))

{-# INLINE descriptorInfoAt #-}

{- |
 Derived lens for retrieving a 'DescriptorInfo` from an IntMap at the given key

 If one does not exist at that key, return a default DescriptorInfo
-}
descriptorInfoAt :: Int -> Lens' (IntMap DescriptorInfo) DescriptorInfo
descriptorInfoAt n =
  lens
    (fromMaybe createDescriptorInfo . IntMap.lookup n)
    (flip (IntMap.insert n))

{-# INLINE fileSelectionTagListModel #-}

{- |
 Derived lens for retrieving the 'FileSelectionTagListModel` from the
 base 'TaggerModel`

 >fileSelectionModel . tagList
-}
fileSelectionTagListModel :: Lens' TaggerModel FileSelectionTagListModel
fileSelectionTagListModel = fileSelectionModel . tagList