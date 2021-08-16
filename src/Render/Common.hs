{-# LANGUAGE OverloadedStrings #-}

module Render.Common where

import Model
import RenderModel

import qualified Data.Text.IO         as T

import Data.Foldable (traverse_)


renderFileProblems :: Printer -> ProblemsAtFileLocation -> IO ()
renderFileProblems printer pfl@(ProblemsAtFileLocation _ _ _ problems) =
  do
    createTitleAndFile printer pfl
    printerParagraph printer
    traverse_  (renderProblem printer) problems


createTitleAndFile :: Printer -> ProblemsAtFileLocation -> IO ()
createTitleAndFile printer (ProblemsAtFileLocation title filePath (s, e) _) =
  let coords = showt s <> ":" <> showt e
      singleFileMessage = "-- " <> showt title <> " ---------- " <> showt filePath <> ":" <> coords
  in (printerTitleColor printer $ singleFileMessage) >> (printerParagraph printer)


renderProblem :: Printer -> ProblemDescription -> IO ()
renderProblem printer (ProblemDescription formatting message) =
  let formattingApplied = traverse_ (printerRenderFormatting printer) formatting
  in formattingApplied >> T.putStr message >> (printerResetAnsi printer)
