module Util (
  head',
) where

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = Just . head $ xs