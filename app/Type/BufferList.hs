{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.BufferList
  ( BufferList (..),
    emptyBufferList,
    buffer,
    list,
    takeToBuffer,
    toBuffer,
    emptyBuffer,
    shuffleBufferList,
    bufferListFromList,
    totalBufferList,
    bufferListNext,
    bufferListPrev,
  )
where

import Control.Lens (Lens', lens, (&), (.~))
import qualified Data.List as L
import Data.Types.Injective (Injective (..))
import IO (initStdGen, shuffle')

data BufferList a = BufferList
  { _buffer :: ![a],
    _list :: ![a]
  }
  deriving (Show, Eq, Ord)

instance Foldable BufferList where
  foldr f a = foldr f a . totalBufferList

instance Semigroup (BufferList a) where
  (BufferList bx xx) <> BufferList {_buffer = by, _list = xy} =
    BufferList (bx <> by) (xx <> xy)

instance Monoid (BufferList a) where
  mempty = emptyBufferList

instance Functor BufferList where
  fmap f (BufferList bs xs) = BufferList (fmap f bs) (fmap f xs)

instance Injective (BufferList a) [a] where
  to = totalBufferList

buffer :: Lens' (BufferList a) [a]
buffer = lens _buffer (\a b -> a {_buffer = b})

list :: Lens' (BufferList a) [a]
list = lens _list (\a b -> a {_list = b})

emptyBufferList :: BufferList a
emptyBufferList = BufferList [] []

totalBufferList :: BufferList a -> [a]
totalBufferList BufferList {_buffer = bs, _list = xs} = bs ++ xs

bufferListFromList :: [a] -> BufferList a
bufferListFromList xs = emptyBufferList & list .~ xs

-- | Loads n items from list to the buffer.
takeToBuffer :: Int -> BufferList a -> BufferList a
takeToBuffer n (BufferList bs xs) =
  let !s = L.splitAt n xs
   in BufferList (bs ++ fst s) (snd s)

-- | Loads all items in list to the buffer.
toBuffer :: BufferList a -> BufferList a
toBuffer (BufferList bs xs) =
  BufferList (bs ++ xs) []

-- | Moves all buffered items to head of the list.
emptyBuffer :: BufferList a -> BufferList a
emptyBuffer (BufferList bs xs) = BufferList [] (bs ++ xs)

shuffleBufferList :: BufferList a -> IO (BufferList a)
shuffleBufferList (BufferList bs xs) = do
  bsh <- shuffle'' bs
  xsh <- shuffle'' xs
  return $ BufferList bsh xsh
  where
    shuffle'' [] = pure []
    shuffle'' xs' = initStdGen >>= return . shuffle' xs' (length xs')

-- |
-- Consumes from the list and pops to buffer.
bufferListNext :: BufferList a -> BufferList a
bufferListNext bl@(BufferList buf xs) =
  case xs of
    [] -> bl
    (x' : xs') -> BufferList (x' : buf) xs'

-- |
-- Consumes from buffer and pops to list.
bufferListPrev :: BufferList a -> BufferList a
bufferListPrev bl@(BufferList buf xs) =
  case buf of
    [] -> bl
    (b' : bufs') -> BufferList bufs' (b' : xs)
