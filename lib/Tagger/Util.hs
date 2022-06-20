{-# OPTIONS_HADDOCK hide #-}

module Tagger.Util (
  head',
  tail',
  hoistMaybe,
  catMaybeTM,
) where

import Control.Monad.Trans.Maybe
import Data.Maybe

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

{- |
 > catMaybeTM f = fmap catMaybes . mapM (runMaybeT . f)
-}
catMaybeTM :: Monad f => (a1 -> MaybeT f a2) -> [a1] -> f [a2]
catMaybeTM f = fmap catMaybes . mapM (runMaybeT . f)