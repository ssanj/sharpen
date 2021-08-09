{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtil where

import Model

import Test.Tasty.HUnit     (assertFailure, Assertion)

import qualified Data.Text          as T
import qualified Data.Text.IO       as T


failWithDecodingError :: String -> Assertion
failWithDecodingError e =
    let errorMessage = show e
    in assertFailure ("Could not decode input: " <> errorMessage)


messageLine :: T.Text ->  Message
messageLine = MessageLine . MessageText


messageFormatGeneralError :: T.Text -> Message
messageFormatGeneralError message = MessageFormatting $ MessageFormat False False (Just "GREEN") message



messageFormatError :: T.Text -> Message
messageFormatError message = MessageFormatting $ MessageFormat False False (Just "RED") message


messageFormatSuggestion :: T.Text -> Message
messageFormatSuggestion message = MessageFormatting $ MessageFormat False False (Just "yellow") message


messageFormatHint :: T.Text -> Message
messageFormatHint message = MessageFormatting $ MessageFormat False True Nothing message


getInput :: String -> IO T.Text
getInput = T.readFile
