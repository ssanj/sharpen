{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DependencyErrorParserSpec where

import Model
import TestUtil

import Test.Tasty.HUnit     ((@?=), Assertion)
import Data.List.NonEmpty   (NonEmpty((:|)))

import qualified Data.Text          as T

unit_decodeDependencyError :: Assertion
unit_decodeDependencyError = do
  input <- getInput "resources/dependency-error.json"
  let decodedE = decodeDependencyError input
  either failWithDecodingError assertDependencyError decodedE


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
