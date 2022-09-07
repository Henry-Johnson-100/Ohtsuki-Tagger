{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal.DescriptorTree (widget) where

import Monomer

widget _ = label_ "Here is the Descriptor Tree" [resizeFactor (-1)]