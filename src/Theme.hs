{-# LANGUAGE OverloadedStrings #-}

module Theme where

import System.Console.ANSI
import Model
import RenderModel

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.Foldable (traverse_)


-- TODO: Make this into a replacable theme

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


renderProblem :: ProblemDescription -> IO ()
renderProblem (ProblemDescription formatting message) =
  let formattingApplied = traverse_ renderFormatting formatting
  in formattingApplied >> T.putStr message >> resetAnsi

renderFileProblems :: ProblemsAtFileLocation -> IO ()
renderFileProblems pfl@(ProblemsAtFileLocation title filePath (s, e) problems) =
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
