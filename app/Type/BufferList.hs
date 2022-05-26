{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.BufferList
  ( BufferList (..),
    Cycleable (..),
    emptyBufferList,
    buffer,
    list,
    takeToBuffer,
    toBuffer,
    emptyBuffer,
    shuffleBufferList,
  )
where

import Control.Lens (Lens', lens)
import qualified Data.List as L
import IO (initStdGen, shuffle')
import Util.Core (head', init', last', tail')

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
  deriving (Show, Eq, Ord)

buffer :: Lens' (BufferList a) [a]
buffer = lens _buffer (\a b -> a {_buffer = b})

list :: Lens' (BufferList a) [a]
list = lens _list (\a b -> a {_list = b})

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

shuffleBufferList :: BufferList a -> IO (BufferList a)
shuffleBufferList (BufferList bs xs) = do
  bsh <- shuffle'' bs
  xsh <- shuffle'' xs
  return $ BufferList bsh xsh
  where
    shuffle'' [] = pure []
    shuffle'' xs' = initStdGen >>= return . shuffle' xs' (length xs')

instance Foldable BufferList where
  foldr f a = foldr f a . cCollect

instance Semigroup (BufferList a) where
  (BufferList bx xx) <> (BufferList by xy) = BufferList (bx <> by) (xx <> xy)

instance Monoid (BufferList a) where
  mempty = emptyBufferList

instance Functor BufferList where
  fmap f (BufferList bs xs) = BufferList (fmap f bs) (fmap f xs)

instance Cycleable [] where
  cPop [] = []
  cPop (x : xs) = xs ++ [x]
  cDequeue [] = []
  cDequeue xs = maybe [] (\x -> x : init' xs) . last' $ xs
  cHead = head'
  cTail [] = Nothing
  cTail (_ : xs) = Just xs
  cCollect = id
  cFromList = id

instance Cycleable BufferList where
  -- cPop consumes the buffer if elements are present and places them in the list.
  cPop bl@(BufferList bs xs) =
    let virtualHead' = head' . cCollect $ bl
     in uncurry BufferList $ case (bs, xs) of
          ([], []) -> ([], [])
          ([], xs') -> ([], cPop xs')
          (bs', xs') -> (tail' bs', maybe xs' (\x -> xs' ++ [x]) virtualHead')

  -- cDequeue consumes the list if elements are present and places them in the buffer.
  cDequeue bl@(BufferList bs xs) =
    let virtualLast' = last' . cCollect $ bl
     in uncurry BufferList $ case (bs, xs) of
          ([], []) -> ([], [])
          (bs', []) -> (cDequeue bs', [])
          (bs', xs') -> (maybe bs' (: bs') virtualLast', init' xs')

  cHead (BufferList bs xs) = head' $ if null bs then xs else bs

  cTail (BufferList [] []) = Nothing
  cTail (BufferList [] (_ : xs)) = Just (BufferList [] xs)
  cTail (BufferList (_ : bs) xs) = Just (BufferList bs xs)

  cCollect = _totalList

  cFromList = BufferList []
