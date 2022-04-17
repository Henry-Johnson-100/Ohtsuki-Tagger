module Node.Color where

import Data.Map.Strict hiding (map)
import Monomer

-- | SHould probably only have 8 colors with the first two being black and white
type ColorMap = Map String Color

newMap :: [Color] -> ColorMap
newMap xs =
  fromList [("Color_black", black), ("Color_white", white)]
    <> (fromList . zipWith (\cix color -> ("Color_" ++ show cix, color)) [2 ..] $ xs)

yuiMap :: ColorMap
yuiMap =
  newMap
    [ yuiLightPeach,
      yuiPeach,
      yuiYellow,
      yuiRed,
      yuiOrange,
      yuiBlue
    ]

yuiPeach :: Color
yuiPeach = rgbHex "#FFECDE"

yuiLightPeach :: Color
yuiLightPeach = rgbHex "#FFF9F6"

yuiYellow :: Color
yuiYellow = rgbHex "#FFE29E"

yuiRed :: Color
yuiRed = rgbHex "#E5444A"

yuiOrange :: Color
yuiOrange = rgbHex "#FF8A44"

yuiBlue :: Color
yuiBlue = rgbHex "#3554A0"