{-# OPTIONS_HADDOCK hide #-}

module Tagger.Util (
  head',
  tail',
  hoistMaybe,
) where

import Control.Monad.Trans.Maybe

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return