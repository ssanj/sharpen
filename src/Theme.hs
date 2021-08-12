{-# LANGUAGE OverloadedStrings #-}

module Theme where

import System.Console.ANSI
import Model
import RenderModel

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.Foldable (traverse_)
import Control.Monad (when)


-- TODO: Make this into a replaceable theme

colorTypeToColor :: ColorType -> Color
colorTypeToColor  CompilerErrorColor      = errorColor
colorTypeToColor  CompilerSuggestionColor = suggestionColor
colorTypeToColor  DependencyErrorColor    = dependencyErrorColor


titleColor :: Color
titleColor = Cyan


errorColor :: Color
errorColor = Red


suggestionColor :: Color
suggestionColor = Yellow


dependencyErrorColor :: Color
dependencyErrorColor = Green


newLines :: Int -> IO ()
newLines n = traverse_ (const $ T.putStrLn "") [1..n]


border ::  IO ()
border = newLines 2


paragraph ::  IO ()
paragraph = newLines 1


renderFormatting :: MessageFormatType -> IO ()
renderFormatting (ColorFormat colorType) =
  let colorsToSGR :: Color -> [SGR]
      colorsToSGR color = [SetColor Foreground Dull color]

      sgrColors :: [SGR]
      sgrColors = colorsToSGR $ colorTypeToColor colorType

  in setSGR sgrColors

renderFormatting UnderlineFormat          = setSGR [SetUnderlining SingleUnderline]
renderFormatting BoldFormat               = setSGR [SetConsoleIntensity BoldIntensity]


resetAnsi :: IO ()
resetAnsi = setSGR [Reset]


withColourInline :: T.Text -> Color -> IO ()
withColourInline text color = do
  setSGR [SetColor Foreground Dull color]
  T.putStr text
  resetAnsi


renderProblem :: ProblemDescription -> IO ()
renderProblem (ProblemDescription formatting message) =
  let formattingApplied = traverse_ renderFormatting formatting
  in formattingApplied >> T.putStr message >> resetAnsi


renderProblem2 :: Printer -> ProblemDescription -> IO ()
renderProblem2 printer (ProblemDescription formatting message) =
  let formattingApplied = traverse_ (printerRenderFormatting printer) formatting
  in formattingApplied >> T.putStr message >> (printerResetAnsi printer)


renderFileProblems :: ProblemsAtFileLocation -> IO ()
renderFileProblems pfl@(ProblemsAtFileLocation _ _ _ problems) =
  do
    createTitleAndFile pfl
    newLines 1
    traverse_  renderProblem problems

renderFileProblems2 :: Printer -> ProblemsAtFileLocation -> IO ()
renderFileProblems2 printer pfl@(ProblemsAtFileLocation _ _ _ problems) =
  do
    createTitleAndFile2 printer pfl
    printerParagraph printer
    traverse_  (renderProblem2 printer) problems


createTitleAndFile :: ProblemsAtFileLocation -> IO ()
createTitleAndFile (ProblemsAtFileLocation title filePath (s, e) _) =
  let coords = showt s <> ":" <> showt e
      singleFileMessage = "-- " <> showt title <> " ---------- " <> showt filePath <> ":" <> coords
  in withColourInline singleFileMessage titleColor >> newLines 1

createTitleAndFile2 :: Printer -> ProblemsAtFileLocation -> IO ()
createTitleAndFile2 printer (ProblemsAtFileLocation title filePath (s, e) _) =
  let coords = showt s <> ":" <> showt e
      singleFileMessage = "-- " <> showt title <> " ---------- " <> showt filePath <> ":" <> coords
  in (printerTitleColor printer $ singleFileMessage) >> (printerParagraph printer)


renderGeneralErrorHeader :: DependencyErrorDescription -> IO ()
renderGeneralErrorHeader (DependencyErrorDescription title path _) =
  let heading  = "-- " <> showt title <> " ---------- " <> showt path
  in withColourInline heading titleColor >> newLines 1


printNumberOfCompilationErrors :: Int -> IO ()
printNumberOfCompilationErrors errors =
  T.putStr "Compilation errors: " >> withColourInline (showt errors) errorColor


printNumberOfCompilationErrors2 :: Printer -> Int -> IO ()
printNumberOfCompilationErrors2 printer errors =
  T.putStr "Compilation errors: " >> printerError printer (showt errors)


renderStats :: Stats -> Int -> IO ()
renderStats stats numberOfErrors =
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors numberOfErrors
    newLines 2

renderStats2 :: Printer -> Stats -> Int -> IO ()
renderStats2 printer stats numberOfErrors =
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors2 printer numberOfErrors
    printerBorder printer




renderCompilerErrorDescription :: CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) errorDescriptions
  newLines 2

renderCompilerErrorDescription2 :: Printer -> CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription2 printer (CompilerErrorDescription errorDescriptions) = do
  let borderX = printerBorder printer
  traverse_ (\ed -> borderX >> renderFileProblems2 printer ed) errorDescriptions
  borderX


renderGeneralProblemsInFile :: DependencyErrorDescription  -> IO ()
renderGeneralProblemsInFile probsInFile =
  do
    border
    renderGeneralErrorHeader probsInFile
    paragraph
    traverse_  renderProblem $ dependencyErrorDescriptionPathProblemDescriptions probsInFile
    border


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
  let withColourInlineX :: T.Text -> Color -> IO ()
      withColourInlineX text color = do
        setSGR [SetColor Foreground Dull color]
        T.putStr text
        resetAnsi

      newLinesX :: Int -> IO ()
      newLinesX n = traverse_ (const $ T.putStrLn "") [1..n]

      colorTypeToColorX :: ColorType -> Color
      colorTypeToColorX  CompilerErrorColor      = colorThemeErrorColor theme
      colorTypeToColorX  CompilerSuggestionColor = colorThemeSuggestionColor theme
      colorTypeToColorX  DependencyErrorColor    = colorThemeDependencyErrorColor theme

      renderFormattingX :: MessageFormatType -> IO ()
      renderFormattingX (ColorFormat colorType) =
        let colorsToSGR :: Color -> [SGR]
            colorsToSGR color = [SetColor Foreground Dull color]

            sgrColors :: [SGR]
            sgrColors = colorsToSGR $ colorTypeToColorX colorType

        in setSGR sgrColors
      renderFormattingX UnderlineFormat          = setSGR [SetUnderlining SingleUnderline]
      renderFormattingX BoldFormat               = setSGR [SetConsoleIntensity BoldIntensity]

  in
    Printer {
      printerBorder           = newLinesX $ colorThemeBorderLines theme
    , printerParagraph        = newLinesX $ colorThemeParagraphLines theme
    , printerTitleColor       = \title ->  withColourInlineX title (colorThemeTitleColor theme) >> newLinesX 1
    , printerError            = \errorStr -> withColourInlineX errorStr (colorThemeErrorColor theme)
    , printerResetAnsi        = setSGR [Reset]
    , printerRenderFormatting = renderFormattingX
    , printerNewLines         = newLinesX
    }

data Printer =
  Printer {
    printerBorder           :: IO ()
  , printerParagraph        :: IO ()
  , printerTitleColor       :: T.Text -> IO ()
  , printerError            :: T.Text -> IO ()
  , printerResetAnsi        :: IO ()
  , printerRenderFormatting :: MessageFormatType -> IO ()
  , printerNewLines         :: Int -> IO ()
  }
