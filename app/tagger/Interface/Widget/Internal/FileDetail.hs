{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal.FileDetail (widget) where

import Monomer

widget _ = label_ "Here is the file Detail" [resizeFactor (-1)]