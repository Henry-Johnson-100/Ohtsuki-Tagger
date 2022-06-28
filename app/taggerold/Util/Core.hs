{-# OPTIONS_GHC -Wno-typed-holes #-}

module Util.Core where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Text as T

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