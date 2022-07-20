module Type.WidgetBuilder.NodeBuilder (
  NodeBuilder (..),
) where

import Control.Monad
import Monomer

data NodeBuilder s e a = NodeBuilder
  {buildWithWidget :: WidgetNode s e -> WidgetNode s e, nodeBuilder :: a}

instance Functor (NodeBuilder s e) where
  fmap = (<$>)

instance Applicative (NodeBuilder s e) where
  pure = return
  (<*>) = ap

instance Monad (NodeBuilder s e) where
  return = NodeBuilder id
  (NodeBuilder bf x) >>= f =
    let (NodeBuilder newBF y) = f x
     in NodeBuilder (bf . newBF) y
