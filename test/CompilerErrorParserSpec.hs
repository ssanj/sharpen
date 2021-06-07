{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompilerErrorParserSpec where

import Model

import Test.Tasty.HUnit     (assertFailure, (@?=), Assertion)
import Data.List.NonEmpty   (NonEmpty((:|)))

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.List.NonEmpty as N


unit_decodeCompilerError :: Assertion
unit_decodeCompilerError = do
  input <- getInput "resources/compiler-error-1.json"
  let decodedE = decodeCompilerError input
  either failWithError assertCompilerError decodedE


unit_decodeGeneralError :: Assertion
unit_decodeGeneralError = do
  input <- getInput "resources/dependency-error.json"
  let decodedE = decodeGeneralError input
  either failWithError assertGeneralError decodedE


failWithError :: String -> Assertion
failWithError e =
    let errorMessage = show e
    in assertFailure ("Could not decode input: " <> errorMessage)


assertGeneralError :: GeneralError -> Assertion
assertGeneralError generalError =

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
  let path = "elm.json"
      title = "ERROR IN DEPENDENCIES"
      message1 = messageLine "It looks like the dependencies elm.json in were edited by hand (or by a 3rd\nparty tool) leaving them in an invalid state.\n\nTry to change them back to what they were before! It is much more reliable to\nadd dependencies with "
      message2 = messageFormat "GREEN" "elm install"
      message3 = messageLine " or the dependency management tool in\n"
      message4 = messageFormat "GREEN" "elm reactor"
      message5 = messageLine ".\n\nPlease ask for help on the community forums if you try those paths and are still\nhaving problems!"
      messages =
          message1 :|
              [
                message2
              , message3
              , message4
              , message5
              ]
      expected = GeneralError path title messages
  in generalError @?= expected


messageLine :: T.Text ->  Message
messageLine = MessageLine . MessageText


messageFormat :: T.Text -> T.Text -> Message
messageFormat color message = MessageFormatting $ MessageFormat False False (Just color) message


messageFormatUnderline :: T.Text -> T.Text -> Message
messageFormatUnderline color message = MessageFormatting $ MessageFormat False True (Just color) message


assertCompilerError :: CompilerError -> Assertion
assertCompilerError compilerError = do
  "compile-errors" @?= compilererrorType compilerError
  let firstProblem = N.head . errorProblems . N.head . compilererrorErrors $ compilerError
  firstProblem @?= namingError


getInput :: String -> IO T.Text
getInput = T.readFile


namingError :: Problem
namingError =

  -- {
  --     "title": "NAMING ERROR",
  --     "region":
  --     {
  --         "start":
  --         {
  --             "line": 293,
  --             "column": 19
  --         },
  --         "end":
  --         {
  --             "line": 293,
  --             "column": 32
  --         }
  --     },
  --     "message":
  --     [
  --         "I cannot find a `NoteSelection` type:\n\n293| choseWhichNotes : NoteSelection -> List SC.NoteFull\n                       ",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "RED",
  --             "string": "^^^^^^^^^^^^^"
  --         },
  --         "\nThese names seem close though:\n\n    ",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "yellow",
  --             "string": "XNoteSelection"
  --         },
  --         "\n    ",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "yellow",
  --             "string": "StorageAction"
  --         },
  --         "\n    ",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "yellow",
  --             "string": "SC.NoteIdVersion"
  --         },
  --         "\n    ",
  --         {
  --             "bold": false,
  --             "underline": false,
  --             "color": "yellow",
  --             "string": "Dom.Element"
  --         },
  --         "\n\n",
  --         {
  --             "bold": false,
  --             "underline": true,
  --             "color": null,
  --             "string": "Hint"
  --         },
  --         ": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm."
  --     ]
  -- },

  let message1  = MessageLine $ MessageText "I cannot find a `NoteSelection` type:\n\n293| choseWhichNotes : NoteSelection -> List SC.NoteFull\n                       "
      message2  = MessageFormatting $ MessageFormat False False (Just "RED") "^^^^^^^^^^^^^"
      message3  = MessageLine $ MessageText "\nThese names seem close though:\n\n    "
      message4  = MessageFormatting $ MessageFormat False False (Just "yellow") "XNoteSelection"
      message5  = MessageLine $ MessageText "\n    "
      message6  = MessageFormatting $ MessageFormat False False (Just "yellow") "StorageAction"
      message7  = MessageLine $ MessageText "\n    "
      message8  = MessageFormatting $ MessageFormat False False (Just "yellow") "SC.NoteIdVersion"
      message9  = MessageLine $ MessageText "\n    "
      message10 = MessageFormatting $ MessageFormat False False (Just "yellow") "Dom.Element"
      message11 = MessageLine $ MessageText "\n\n"
      message12 = MessageFormatting $ MessageFormat False True Nothing "Hint"
      message13 = MessageLine $ MessageText ": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm."
      messages  =
          message1 :|
              [
                message2
              , message3
              , message4
              , message5
              , message6
              , message7
              , message8
              , message9
              , message10
              , message11
              , message12
              , message13
              ]

      regionStart = LineAndColumn 293 19
      regionEnd   = LineAndColumn 293 32
      region      = Region regionStart regionEnd


      problem = Problem "NAMING ERROR" region messages
  in problem


