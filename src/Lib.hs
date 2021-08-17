{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( sharpen
    ) where

import System.IO hiding (FilePath)
import Prelude hiding (FilePath)

import Model

import qualified Data.Text                                 as T
import qualified Data.Text.IO                              as T
import qualified DependencyErrorProcessor                  as DEP
import qualified ElmCompilerErrorProcessor                 as CE
import qualified Render.CompilerErrorDescriptionRenderer   as RCE
import qualified Render.DependencyErrorDescriptionRenderer as RDE


sharpen :: RuntimeConfig -> IO ()
sharpen runtimeConfig = do
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
  let colorTheme = runtimeConfigColorTheme rc
  in
    case elmCompilerOutput of
      ElmError compilerError -> RCE.render colorTheme $ CE.processError rc compilerError
      OtherError otherError  -> RDE.render colorTheme $ DEP.processError rc otherError

