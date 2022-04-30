{-# LANGUAGE TemplateHaskell #-}

module Type.BufferList
  ( BufferList (..),
    Cycleable (..),
    emptyBufferList,
    buffer,
    list,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Maybe (fromMaybe)

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

instance Functor BufferList where
  fmap f (BufferList bs xs) = BufferList (fmap f bs) (fmap f xs)

instance Cycleable BufferList where
  -- For (BufferList bs xs)
  --
  -- If bs is empty then pop xs
  --
  -- Else, move the head of the buffer to the end of xs
  cPop (BufferList bs xs) =
    case bs of
      [] -> case xs of
        x' : xs' -> BufferList [] (xs' ++ [x'])
        [] -> emptyBufferList
      b' : bs' -> BufferList bs' (xs ++ [b'])

  --  For (BufferList bs xs)
  --
  -- If xs is empty then dequeue the buffer
  --
  -- Else, move the last item in xs to the head of the buffer.
  cDequeue (BufferList bs xs) =
    case xs of
      [] -> case bs of
        [] -> emptyBufferList
        bs' -> maybe emptyBufferList (\b' -> BufferList (b' : tail' bs') []) . last' $ bs'
      xs' -> maybe emptyBufferList (\x' -> BufferList [x'] (tail' xs')) . last' $ xs'

  cHead (BufferList bs xs) = head' $ if null bs then xs else bs
  cTail (BufferList [] []) = Nothing
  cTail (BufferList [] (x : xs)) = Just (BufferList [] xs)
  cTail (BufferList (b : bs) xs) = Just (BufferList bs xs)
  cCollect = _totalList
