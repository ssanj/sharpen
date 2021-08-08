{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( sharpen
    ) where

import System.IO hiding (FilePath)
import Prelude hiding (FilePath)

import Model
import ColorMap
import RenderModel
import Theme (renderGeneralProblemsInFile, renderStats, renderStats, renderCompilerErrorDescription)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import DependencyErrorProcessor       as DEP
import ElmCompilerErrorProcessor      as CE

sharpen :: Config -> IO ()
sharpen config = do
  let runtimeConfig = RuntimeConfig config allColorNamesMap
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = decodeElmCompilerOutput content :: Either String ElmCompilerOutput

        errorIO :: String -> IO ()
        errorIO = T.putStrLn . ("Parsing error: " <>) . T.pack

        successIO :: ElmCompilerOutput -> IO ()
        successIO = simplePrinter runtimeConfig

    in either errorIO successIO resultE


simplePrinter :: RuntimeConfig -> ElmCompilerOutput -> IO ()
simplePrinter rc elmCompilerOutput =
  case elmCompilerOutput of
    ElmError compilerError  -> handleCompilerError $ CE.processError rc compilerError
    OtherError generalError -> handleGeneralError $ DEP.processError rc generalError

handleCompilerError :: CompilerErrorRenderModel -> IO ()
handleCompilerError compilerErrorRenderModel =
  let
      compilerErrorDesc    = compilerErrorRenderModelCompilerErrorDescription compilerErrorRenderModel
      numProblemsDisplayed = compilerErrorRenderModelProblemsToDisplay compilerErrorRenderModel
      stats                = compilerErrorRenderModelStats compilerErrorRenderModel

      renderEnabledStats   = renderStats stats numProblemsDisplayed
      renderCompilerErrors = renderCompilerErrorDescription compilerErrorDesc
  in renderCompilerErrors >> renderEnabledStats

handleGeneralError :: DependencyErrorDescription -> IO ()
handleGeneralError = renderGeneralProblemsInFile
