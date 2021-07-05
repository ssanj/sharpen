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
      colorsToSGR color = [ SetColor Foreground Dull color]

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


renderFileProblems :: ProblemsAtFileLocation -> IO ()
renderFileProblems pfl@(ProblemsAtFileLocation _ _ _ problems) =
  do
    createTitleAndFile pfl
    newLines 1
    traverse_  renderProblem problems


createTitleAndFile :: ProblemsAtFileLocation -> IO ()
createTitleAndFile (ProblemsAtFileLocation title filePath (s, e) _) =
  let coords = showt s <> ":" <> showt e
      singleFileMessage = "-- " <> showt title <> " ---------- " <> showt filePath <> ":" <> coords
  in withColourInline singleFileMessage titleColor >> newLines 1


renderGeneralErrorHeader :: GeneralProblemsInFile -> IO ()
renderGeneralErrorHeader (GeneralProblemsInFile title path _) =
  let heading  = "-- " <> showt title <> " ---------- " <> showt path
  in withColourInline heading titleColor >> newLines 1


printNumberOfCompilationErrors :: Int -> IO ()
printNumberOfCompilationErrors errors =
  T.putStr "Compilation errors: " >> withColourInline (showt errors) errorColor


renderStats :: Stats -> Int -> IO ()
renderStats stats numberOfErrors =
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors numberOfErrors
    newLines 2


renderCompilerErrorDescription :: CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) errorDescriptions
  newLines 2


renderGeneralProblemsInFile :: GeneralProblemsInFile  -> IO ()
renderGeneralProblemsInFile probsInFile =
  do
    border
    renderGeneralErrorHeader probsInFile
    paragraph
    traverse_  renderProblem $ generalProblemsInFilePathProblemDescriptions probsInFile
    border
