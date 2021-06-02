module ColorMap where

import Model (ColorMap, MessageFormatType(..), showt)
import System.Console.ANSI
import System.Console.ANSI.Types (Color)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
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

renderFormatting :: MessageFormatType -> IO ()
renderFormatting (ColourFormat color) = setSGR [SetColor Foreground Dull color]
renderFormatting UnderlineFormat      = setSGR [SetUnderlining SingleUnderline]
renderFormatting BoldFormat           = setSGR [SetConsoleIntensity BoldIntensity]


resetAnsi :: IO ()
resetAnsi = setSGR [Reset]

titleColor :: Color
titleColor = Cyan

errorColor :: Color
errorColor = Red


withColourInline :: T.Text -> Color -> IO ()
withColourInline text color = do
  setSGR [SetColor Foreground Dull color]
  T.putStr text
  resetAnsi
