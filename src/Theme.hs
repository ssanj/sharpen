{-# LANGUAGE OverloadedStrings #-}

module Theme where

import System.Console.ANSI
import Model
import RenderModel

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.Foldable (traverse_)
import Control.Monad (when)


renderProblem2 :: Printer -> ProblemDescription -> IO ()
renderProblem2 printer (ProblemDescription formatting message) =
  let formattingApplied = traverse_ (printerRenderFormatting printer) formatting
  in formattingApplied >> T.putStr message >> (printerResetAnsi printer)



renderFileProblems2 :: Printer -> ProblemsAtFileLocation -> IO ()
renderFileProblems2 printer pfl@(ProblemsAtFileLocation _ _ _ problems) =
  do
    createTitleAndFile2 printer pfl
    printerParagraph printer
    traverse_  (renderProblem2 printer) problems


createTitleAndFile2 :: Printer -> ProblemsAtFileLocation -> IO ()
createTitleAndFile2 printer (ProblemsAtFileLocation title filePath (s, e) _) =
  let coords = showt s <> ":" <> showt e
      singleFileMessage = "-- " <> showt title <> " ---------- " <> showt filePath <> ":" <> coords
  in (printerTitleColor printer $ singleFileMessage) >> (printerParagraph printer)



printNumberOfCompilationErrors2 :: Printer -> Int -> IO ()
printNumberOfCompilationErrors2 printer errors =
  T.putStr "Compilation errors: " >> printerError printer (showt errors)


renderStats2 :: Printer -> Stats -> Int -> IO ()
renderStats2 printer stats numberOfErrors =
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors2 printer numberOfErrors
    printerBorder printer


renderCompilerErrorDescription2 :: Printer -> CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription2 printer (CompilerErrorDescription errorDescriptions) = do
  let borderX = printerBorder printer
  traverse_ (\ed -> borderX >> renderFileProblems2 printer ed) errorDescriptions
  borderX


renderGeneralErrorHeader2 :: Printer -> DependencyErrorDescription -> IO ()
renderGeneralErrorHeader2 printer (DependencyErrorDescription title path _) =
  let heading     = "-- " <> showt title <> " ---------- " <> showt path
      titleColor2 = printerTitleColor printer $ heading
      paragraph3  = printerParagraph printer
  in titleColor2 >> paragraph3


renderGeneralProblemsInFile2 :: Printer -> DependencyErrorDescription  -> IO ()
renderGeneralProblemsInFile2 printer probsInFile =
  do
    printerBorder printer
    renderGeneralErrorHeader2 printer probsInFile
    printerParagraph printer
    traverse_  (renderProblem2 printer) $ dependencyErrorDescriptionPathProblemDescriptions probsInFile
    printerBorder printer


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
