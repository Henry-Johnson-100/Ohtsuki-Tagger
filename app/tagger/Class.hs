{-# LANGUAGE MultiParamTypeClasses #-}

module Class (
  Isomorphic (..),
) where

{- |
 Ignoring any genuine requirements for bijectivity.

 as deriving that many typeclasses would be annoying.

 Though in the future, it should be relatively easy to derive any necessary
 instances for newtypes via templates.
-}
class Isomorphic a b where
  to :: a -> b
  from :: b -> a