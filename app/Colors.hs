module Colors where

import Model (ColorTheme(..))

import System.Console.ANSI

defaultTheme :: ColorTheme
defaultTheme =
  ColorTheme {
    colorThemeTitleColor           = Cyan
  , colorThemeErrorColor           = Red
  , colorThemeSuggestionColor      = Yellow
  , colorThemeDependencyErrorColor = Green
  , colorThemeBorderLines          = 2   -- How many newlines
  , colorThemeParagraphLines       = 1   -- How many newlines
  }
