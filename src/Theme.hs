{-# LANGUAGE OverloadedStrings #-}

module Theme where


import System.Console.ANSI
import Model

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.Foldable (traverse_)

fromTheme :: ColorTheme -> Printer
fromTheme theme =
  let resetAnsi :: IO ()
      resetAnsi = setSGR [Reset]

      withColourInline :: T.Text -> Color -> IO ()
      withColourInline text color = do
        setSGR [SetColor Foreground Dull color]
        T.putStr text
        resetAnsi

      newLines :: Int -> IO ()
      newLines n = traverse_ (const $ T.putStrLn "") [1..n]

      colorTypeToColor :: ColorType -> Color
      colorTypeToColor  CompilerErrorColor      = colorThemeErrorColor theme
      colorTypeToColor  CompilerSuggestionColor = colorThemeSuggestionColor theme
      colorTypeToColor  DependencyErrorColor    = colorThemeDependencyErrorColor theme

      renderFormatting :: MessageFormatType -> IO ()
      renderFormatting (ColorFormat colorType) =
        let colorsToSGR :: Color -> [SGR]
            colorsToSGR color = [SetColor Foreground Dull color]

            sgrColors :: [SGR]
            sgrColors = colorsToSGR $ colorTypeToColor colorType

        in setSGR sgrColors
      renderFormatting UnderlineFormat          = setSGR [SetUnderlining SingleUnderline]
      renderFormatting BoldFormat               = setSGR [SetConsoleIntensity BoldIntensity]

  in
    Printer {
      printerBorder           = newLines $ colorThemeBorderLines theme
    , printerParagraph        = newLines $ colorThemeParagraphLines theme
    , printerTitleColor       = \title ->  withColourInline title (colorThemeTitleColor theme) >> newLines 1
    , printerError            = \errorStr -> withColourInline errorStr (colorThemeErrorColor theme)
    , printerResetAnsi        = setSGR [Reset]
    , printerRenderFormatting = renderFormatting
    , printerNewLines         = newLines
    }
