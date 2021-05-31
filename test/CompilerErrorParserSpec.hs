{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompilerErrorParserSpec where

import Model

import Lib                  (decodeInput)
import Test.Tasty.HUnit     (assertFailure, (@?=), Assertion)


import qualified Data.Text     as T
import qualified Data.Text.IO  as T


unit_decodeCompilerError :: Assertion
unit_decodeCompilerError = do
    input <- getInput "resources/compiler-error-1.json"
    let decodedE = decodeInput input :: Either String CompilerOutput
    either failWithError assertCompilerOutput decodedE


failWithError :: String -> Assertion
failWithError e =
    let errorMessage = show e
    in assertFailure ("Could not decode input: " <> errorMessage)


assertCompilerOutput :: CompilerOutput -> Assertion
assertCompilerOutput co = do
    "compile-errors" @?= (compileroutputType co)


getInput :: String -> IO T.Text
getInput = T.readFile
