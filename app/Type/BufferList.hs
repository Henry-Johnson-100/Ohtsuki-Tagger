{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.BufferList
  ( BufferList (..),
    Cycleable (..),
    emptyBufferList,
    buffer,
    list,
    takeToBuffer,
    toBuffer,
    emptyBuffer,
  )
where

import Control.Lens.TH (makeLenses)
import qualified Data.List as L
import Data.Maybe

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = Just . head $ xs

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : xs) = xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just . last $ xs

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

class Cycleable c where
  -- | Take the head item and move it to the back
  cPop :: c a -> c a

  -- | Take the last item and move it to the front
  cDequeue :: c a -> c a

  -- | Maybe get the head item
  cHead :: c a -> Maybe a

  -- | Maybe get the remainder of the Cycleable
  cTail :: c a -> Maybe (c a)

  -- | Collect the Cycleable as a list of elements
  cCollect :: c a -> [a]

  cFromList :: [a] -> c a

data BufferList a = BufferList
  { _buffer :: ![a],
    _list :: ![a]
  }
  deriving (Show, Eq)

makeLenses ''BufferList

emptyBufferList :: BufferList a
emptyBufferList = BufferList [] []

_totalList :: BufferList a -> [a]
_totalList (BufferList bs xs) = bs ++ xs

-- | Loads n items from list to the buffer.
takeToBuffer :: Int -> BufferList a -> BufferList a
takeToBuffer n (BufferList bs xs) =
  let !s = L.splitAt n xs
   in BufferList (bs ++ fst s) (snd s)

-- | Loads all items in list to the buffer.
toBuffer :: BufferList a -> BufferList a
toBuffer (BufferList bs xs) = BufferList (bs ++ xs) []

-- | Moves all buffered items to head of the list.
emptyBuffer :: BufferList a -> BufferList a
emptyBuffer (BufferList bs xs) = BufferList [] (bs ++ xs)

instance Functor BufferList where
  fmap f (BufferList bs xs) = BufferList (fmap f bs) (fmap f xs)

instance Cycleable [] where
  cPop [] = []
  cPop (x : xs) = xs ++ [x]
  cDequeue [] = []
  cDequeue xs = maybe [] (\x -> x : init' xs) . last' $ xs
  cHead = head'
  cTail [] = Nothing
  cTail (x : xs) = Just xs
  cCollect = id
  cFromList = id

instance Cycleable BufferList where
  cPop (BufferList bs xs) = uncurry BufferList $
    case (bs, xs) of
      ([], []) -> ([], [])
      (bs, []) -> (cPop bs, [])
      (bs, xs) -> (head xs : bs, tail' xs)

  cDequeue (BufferList bs xs) = uncurry BufferList $
    case (bs, xs) of
      ([], []) -> ([], [])
      (bs, []) -> (cDequeue bs, [])
      ([], xs) -> (maybeToList . last' $ xs, init' xs)
      (bs, xs) -> (maybe bs (: bs) . last' $ xs, init' xs)

  cHead (BufferList bs xs) = head' $ if null bs then xs else bs
  cTail (BufferList [] []) = Nothing
  cTail (BufferList [] (x : xs)) = Just (BufferList [] xs)
  cTail (BufferList (b : bs) xs) = Just (BufferList bs xs)
  cCollect = _totalList
  cFromList xs = BufferList [] xs
