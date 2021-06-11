{-# LANGUAGE OverloadedStrings #-}

module Theme where

import System.Console.ANSI
import Model

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.Foldable (traverse_)


titleColor :: Color
titleColor = Cyan


errorColor :: Color
errorColor = Red


newLines :: Int -> IO ()
newLines n = traverse_ (const $ T.putStrLn "") [1..n]


border ::  IO ()
border = newLines 2


paragraph ::  IO ()
paragraph = newLines 1


renderFormatting :: MessageFormatType -> IO ()
renderFormatting (ColourFormat color) = setSGR [SetColor Foreground Dull color]
renderFormatting UnderlineFormat      = setSGR [SetUnderlining SingleUnderline]
renderFormatting BoldFormat           = setSGR [SetConsoleIntensity BoldIntensity]


resetAnsi :: IO ()
resetAnsi = setSGR [Reset]


withColourInline :: T.Text -> Color -> IO ()
withColourInline text color = do
  setSGR [SetColor Foreground Dull color]
  T.putStr text
  resetAnsi