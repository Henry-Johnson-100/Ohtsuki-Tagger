{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Model.Shared.Core (
  Visibility (..),
  toggleAltVis,
  setPaneVis,
  unsetPaneVis,
  hasVis,
  togglePaneVis,
  OrderDirection (..),
  OrderCriteria (..),
  OrderBy (..),
  cycleOrderCriteria,
  cycleOrderDir,
  TextHistory (..),
  createHistory,
  nextHist,
  prevHist,
  putHist,
  getHist,
) where

import Data.Sequence (Seq ((:<|)), (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tagger (CyclicEnum (next))
import Data.Text (Text)
import qualified Data.Text as T

{- |
 Generic data type for changing visibility of a widget.

 Provides labels for visibility for a main page and alternate page and
 two additional constructors for either numbered pages or labeled pages.
-}
data Visibility
  = VisibilityMain
  | VisibilityAlt
  | VisibilityNum Int
  | VisibilityLabel Text
  | VisibilityPanes Visibility (Set Visibility)
  deriving (Show, Eq, Ord)

setPaneVis :: Visibility -> Visibility -> Visibility
setPaneVis x y =
  case x of
    VisibilityPanes x' s -> VisibilityPanes x' (S.insert y s)
    _ -> VisibilityPanes x (S.singleton y)

unsetPaneVis :: Visibility -> Visibility -> Visibility
unsetPaneVis x y =
  case x of
    VisibilityPanes x' s ->
      let newVisSet = S.delete y s
       in if S.null newVisSet then x' else VisibilityPanes x' newVisSet
    _ -> x

hasVis :: Visibility -> Visibility -> Bool
hasVis x y =
  case x of
    VisibilityPanes x' s -> x' == y || S.member y s
    _ -> x == y

{- |
 Switches between Main and Alt visibility.

 Does nothing if the visibility is not either of these two constructors.
-}
toggleAltVis :: Visibility -> Visibility
toggleAltVis VisibilityAlt = VisibilityMain
toggleAltVis VisibilityMain = VisibilityAlt
toggleAltVis (VisibilityPanes x ps) = VisibilityPanes (toggleAltVis x) ps
toggleAltVis x = x

togglePaneVis :: Visibility -> Visibility -> Visibility
togglePaneVis x y =
  if x `hasVis` y
    then unsetPaneVis x y
    else setPaneVis x y

data OrderDirection = Asc | Desc
  deriving
    (Show, Eq, Ord, Enum, Bounded, CyclicEnum)

data OrderCriteria = Alphabetic | Numeric
  deriving
    (Show, Eq, Ord, Enum, Bounded, CyclicEnum)

data OrderBy = OrderBy
  { _orderbyOrderCriteria :: OrderCriteria
  , _orderbyOrderDirection :: OrderDirection
  }
  deriving (Show, Eq)

cycleOrderCriteria :: OrderBy -> OrderBy
cycleOrderCriteria (OrderBy c d) = OrderBy (next c) d

cycleOrderDir :: OrderBy -> OrderBy
cycleOrderDir (OrderBy c d) = OrderBy c (next d)

data TextHistory = TextHistory
  { _textHistorySize :: Int
  , _textHistoryIndex :: Int
  , _textHistoryContents :: Seq Text
  }
  deriving (Show, Eq)

createHistory :: Int -> TextHistory
createHistory n = TextHistory n 0 Seq.empty

getHist :: TextHistory -> Maybe Text
getHist (TextHistory _ ix h) = Seq.lookup ix h

nextHist :: TextHistory -> TextHistory
nextHist (TextHistory n ix h) =
  TextHistory
    n
    ( if ix >= n
        then n
        else
          let histSize = Seq.length h - 1
           in if ix >= histSize then histSize else ix + 1
    )
    h

prevHist :: TextHistory -> TextHistory
prevHist (TextHistory n ix h) =
  TextHistory n (if ix <= 0 then 0 else ix - 1) h

putHist :: Text -> TextHistory -> TextHistory
putHist t th@(TextHistory n ix h)
  | T.null t = th
  | Seq.length h >= (n - 1) = case h of
    Seq.Empty -> TextHistory n ix h
    (_ :<| h') -> TextHistory n ix (t <| h')
  | otherwise = TextHistory n ix (t <| h)
