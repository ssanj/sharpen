{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( sharpen
    ) where

import System.IO hiding (FilePath)
import Prelude hiding (FilePath)

import Model
import ColorMap (allColorNamesMap)

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import DependencyErrorProcessor                as DEP
import ElmCompilerErrorProcessor               as CE
import Render.CompilerErrorDescriptionRenderer as RCE
import Render.DependencyErrorDescriptionRenderer as RDE


sharpen :: ColorTheme -> Config -> IO ()
sharpen colorTheme config = do
  let runtimeConfig = RuntimeConfig config allColorNamesMap colorTheme
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
      ElmError compilerError -> RCE.render $ CE.processError rc compilerError
      OtherError otherError  -> RDE.render colorTheme $ DEP.processError rc otherError

