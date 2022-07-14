module Util (
  head',
  both,
) where

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = Just . head $ xs

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)