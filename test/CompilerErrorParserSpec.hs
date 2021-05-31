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
    either (\e -> assertFailure ("Could not decode input: " <> show e)) (\_ -> True @?= True) decodedE

getInput :: String -> IO T.Text
getInput = T.readFile
