{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

module Interface.Widget.Internal.Composite.QueryBuilder () where

import Data.Tagger (SetOp (..))
import Data.Text (Text)
import Text.TaggerQL.Expression.AST (
  Expression (..),
  FileTerm (FileTerm),
  SubExpression (..),
  TagTerm (..),
 )

isNestedExpr :: Expression -> Bool
isNestedExpr (Binary _ _ _) = True
isNestedExpr _ = False

isNestedSubExpr :: SubExpression -> Bool
isNestedSubExpr (SubBinary _ _ _) = True
isNestedSubExpr _ = False

formatTagTerm :: TagTerm -> Text
formatTagTerm tt = case tt of
  DescriptorTerm txt -> "d." <> txt
  MetaDescriptorTerm txt -> txt

formatSetOp :: SetOp -> Text
formatSetOp so = case so of
  Union -> "|"
  Intersect -> "&"
  Difference -> "!"

formatSubExpr :: SubExpression -> Text
formatSubExpr subExpr = case subExpr of
  SubTag tt -> formatTagTerm tt
  SubBinary se so se' ->
    (if isNestedSubExpr se then (\t -> "(" <> t <> ") ") else (<> " ")) (formatSubExpr se)
      <> formatSetOp so
      <> (if isNestedSubExpr se' then (\t -> " (" <> t <> ")") else (" " <>))
        (formatSubExpr se')
  SubExpression tt se -> formatTagTerm tt <> " {" <> formatSubExpr se <> "}"

formatExpr :: Expression -> Text
formatExpr expr = case expr of
  FileTermValue (FileTerm p) -> "p." <> p
  TagTermValue tt -> formatTagTerm tt
  TagExpression tt se -> formatTagTerm tt <> " {" <> formatSubExpr se <> "}"
  Binary ex so ex' ->
    (if isNestedExpr ex then (\t -> "(" <> t <> ") ") else (<> " ")) (formatExpr ex)
      <> formatSetOp so
      <> (if isNestedExpr ex' then (\t -> " (" <> t <> ")") else (" " <>))
        (formatExpr ex')