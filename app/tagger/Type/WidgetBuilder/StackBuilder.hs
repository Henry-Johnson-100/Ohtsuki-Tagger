{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.WidgetBuilder.StackBuilder (
  StackBuilder (..),
  runStackBuilder,
  stack,
) where

import Control.Monad (ap)
import Data.Sequence
import Data.Text (Text)
import Monomer (LabelCfg, WidgetNode, label_)

data StackBuilder s e a = StackBuilder
  { stackBuilderStack :: Seq (WidgetNode s e)
  , stackBuilder :: a
  }
  deriving (Functor)

instance Applicative (StackBuilder s e) where
  pure = return
  (<*>) = ap

instance Monad (StackBuilder s e) where
  return = StackBuilder empty
  (StackBuilder s x) >>= f =
    let (StackBuilder t y) = f x
     in StackBuilder (s >< t) y

runStackBuilder ::
  (Seq (WidgetNode s e) -> WidgetNode s e) ->
  StackBuilder s e a ->
  WidgetNode s e
runStackBuilder f (StackBuilder s _) = f s

stack :: WidgetNode s e -> StackBuilder s e ()
stack = flip StackBuilder () . singleton