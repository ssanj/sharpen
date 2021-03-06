module ColorMap where

import Model (ColorMap, showt)
import System.Console.ANSI

import qualified Data.Text            as T
import qualified Data.Map.Strict      as M

allColorNamesMap :: ColorMap
allColorNamesMap =
  let allColors     = enumFromTo (minBound::Color) (maxBound:: Color)
      colorNamePair = (\color -> (T.toUpper . showt $ color, color)) <$> allColors
  in M.fromList colorNamePair


maybeColor :: ColorMap -> T.Text -> Maybe Color
maybeColor colorNamesMap text =
  let upperText     = T.toUpper text
  in M.lookup upperText colorNamesMap
