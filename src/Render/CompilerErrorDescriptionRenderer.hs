{-# LANGUAGE OverloadedStrings #-}

module Render.CompilerErrorDescriptionRenderer (render) where

import Model         (ColorTheme(..), Stats(..), Printer(..), showt)
import RenderModel   (CompilerErrorRenderModel(..), CompilerErrorDescription(..))
import Theme         (fromTheme)
import Render.Common (renderFileProblems)
import Data.Foldable (traverse_)
import Control.Monad (when)

import qualified Data.Text.IO         as T

render :: ColorTheme -> CompilerErrorRenderModel -> IO ()
render colorTheme compilerErrorRenderModel =
  let
      compilerErrorDesc    = compilerErrorRenderModelCompilerErrorDescription compilerErrorRenderModel
      numProblemsDisplayed = compilerErrorRenderModelProblemsToDisplay compilerErrorRenderModel
      stats                = compilerErrorRenderModelStats compilerErrorRenderModel
      printer              = fromTheme colorTheme
      renderEnabledStats   = renderStats printer stats numProblemsDisplayed
      renderCompilerErrors = renderCompilerErrorDescription printer compilerErrorDesc
  in renderCompilerErrors >> renderEnabledStats


renderStats :: Printer -> Stats -> Int -> IO ()
renderStats printer stats numberOfErrors =
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors printer numberOfErrors
    printerBorder printer


printNumberOfCompilationErrors :: Printer -> Int -> IO ()
printNumberOfCompilationErrors printer errors =
  T.putStr "Compilation errors: " >> printerError printer (showt errors)


renderCompilerErrorDescription :: Printer -> CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription printer (CompilerErrorDescription errorDescriptions) = do
  let borderX = printerBorder printer
  traverse_ (\ed -> borderX >> renderFileProblems printer ed) errorDescriptions
  borderX

