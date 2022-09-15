{-# OPTIONS_HADDOCK hide #-}

module Tagger.Util (
  head',
  tail',
  last',
  hoistMaybe,
  catMaybeTM,
) where

import Control.Monad.Trans.Maybe
import qualified Data.Foldable as F
import Data.Maybe
import Data.Sequence (Seq, (<|), (><))
import qualified Data.Sequence as Seq

{-# INLINE head' #-}
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

{-# INLINE tail' #-}
tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

{-# INLINE last' #-}
last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just . last $ xs

{-# INLINE hoistMaybe #-}
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

{-# INLINE catMaybeTM #-}

{- |
 > catMaybeTM f = fmap catMaybes . mapM (runMaybeT . f)
-}
catMaybeTM :: Monad f => (a1 -> MaybeT f a2) -> [a1] -> f [a2]
catMaybeTM f = fmap catMaybes . mapM (runMaybeT . f)
