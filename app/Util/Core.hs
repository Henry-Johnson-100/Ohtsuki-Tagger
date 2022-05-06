{-# LANGUAGE TupleSections #-}

module Util.Core where

import Control.Monad.Trans.Maybe
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T

type OccurrenceMap a = IntMap.IntMap Int

class PrimaryKey s where
  getId :: s -> Int
  foldOccurrences :: [s] -> OccurrenceMap s
  foldOccurrences = makeOccurrenceMap . map getId
  decodeOccurrences :: [s] -> [(s, Int)]
  decodeOccurrences ss =
    zipDecodeOccurrenceMap
      (map (\s -> (getId s, s)) ss)
      (foldOccurrences ss)
  decodeOccurrencesWith :: [s] -> OccurrenceMap s -> [(s, Int)]
  decodeOccurrencesWith ss = zipDecodeOccurrenceMap (map (\s -> (getId s, s)) ss)

makeOccurrenceMap :: [Int] -> OccurrenceMap a
makeOccurrenceMap [] = IntMap.empty
makeOccurrenceMap (x : xs) =
  let m = makeOccurrenceMap xs
   in maybe
        (IntMap.insert x 1 m)
        (\v -> IntMap.insert x (succ v) m)
        (IntMap.lookup x m)

zipDecodeOccurrenceMap :: [(Int, a)] -> OccurrenceMap a -> [(a, Int)]
zipDecodeOccurrenceMap [] _ = []
zipDecodeOccurrenceMap xs m = mapMaybe (\(n, x) -> IntMap.lookup n m >>= Just . (x,)) xs

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : xs) = xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just . last $ xs

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

(!++) :: T.Text -> T.Text -> T.Text
(!++) = T.append

maybeWithList :: (a -> [b]) -> Maybe a -> [b]
maybeWithList = maybe []