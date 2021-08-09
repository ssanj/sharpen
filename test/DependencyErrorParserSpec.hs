{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DependencyErrorParserSpec where

import Model

import Test.Tasty.HUnit     (assertFailure, (@?=), Assertion)
import Data.List.NonEmpty   (NonEmpty((:|)))

import qualified Data.Text          as T
import qualified Data.Text.IO       as T


unit_decodeDependencyError :: Assertion
unit_decodeDependencyError = do
  input <- getInput "resources/dependency-error.json"
  let decodedE = decodeDependencyError input
  either failWithError assertDependencyError decodedE


failWithError :: String -> Assertion
failWithError e =
    let errorMessage = show e
    in assertFailure ("Could not decode input: " <> errorMessage)


assertDependencyError :: DependencyError -> Assertion
assertDependencyError generalError =

  -- {
  --     "type": "error",
  --     "path": "elm.json",
  --     "title": "ERROR IN DEPENDENCIES",
  --     "message":
  --     [
  --         "It looks like the dependencies elm.json in were edited by hand (or by a 3rd\nparty tool) leaving them in an invalid state.\n\nTry to change them back to what they were before! It is much more reliable to\nadd dependencies with ",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "GREEN",
  --             "string": "elm install"
  --         },
  --         " or the dependency management tool in\n",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "GREEN",
  --             "string": "elm reactor"
  --         },
  --         ".\n\nPlease ask for help on the community forums if you try those paths and are still\nhaving problems!"
  --     ]
  -- }
  let path  = "elm.json" :: T.Text
      title = "ERROR IN DEPENDENCIES" :: T.Text

      message1 = messageLine "It looks like the dependencies elm.json in were edited by hand (or by a 3rd\nparty tool) leaving them in an invalid state.\n\nTry to change them back to what they were before! It is much more reliable to\nadd dependencies with "
      message2 = messageFormatGeneralError "elm install"
      message3 = messageLine " or the dependency management tool in\n"
      message4 = messageFormatGeneralError "elm reactor"
      message5 = messageLine ".\n\nPlease ask for help on the community forums if you try those paths and are still\nhaving problems!"
      messages =
          message1 :|
              [
                message2
              , message3
              , message4
              , message5
              ]
      expected = DependencyError path title messages
  in generalError @?= expected


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
