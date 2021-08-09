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


failWithError :: String -> Assertion
failWithError e =
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


assertCompilerError :: CompilerError -> Assertion
assertCompilerError compilerError = do
  "compile-errors" @?= compilererrorType compilerError
  let problems = errorProblems . N.head . compilererrorErrors $ compilerError
  problems @?= namingError1  :| [ namingError2 ]


getInput :: String -> IO T.Text
getInput = T.readFile


namingError1 :: Problem
namingError1 =

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

  let message1  = messageLine "I cannot find a `NoteSelection` type:\n\n293| choseWhichNotes : NoteSelection -> List SC.NoteFull\n                       "
      message2  = messageFormatError "^^^^^^^^^^^^^"
      message3  = messageLine "\nThese names seem close though:\n\n    "
      message4  = messageFormatSuggestion "XNoteSelection"
      message5  = messageLine "\n    "
      message6  = messageFormatSuggestion "StorageAction"
      message7  = messageLine "\n    "
      message8  = messageFormatSuggestion "SC.NoteIdVersion"
      message9  = messageLine "\n    "
      message10 = messageFormatSuggestion "Dom.Element"
      message11 = messageLine "\n\n"
      message12 = messageFormatHint "Hint"
      message13 = messageLine ": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm."
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

      _regionStart = LineAndColumn 293 19
      _regionEnd   = LineAndColumn 293 32
      _region      = Region _regionStart _regionEnd

  in Problem "NAMING ERROR" _region messages

namingError2 :: Problem
namingError2 =

  -- {
  --     "title": "NAMING ERROR",
  --     "region":
  --     {
  --         "start":
  --         {
  --             "line": 285,
  --             "column": 27
  --         },
  --         "end":
  --         {
  --             "line": 285,
  --             "column": 40
  --         }
  --     },
  --     "message":
  --     [
  --         "I cannot find a `NoteSelection` type:\n\n285| noteSelection  : Model -> NoteSelection\n                               ",
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
  -- }

  let message1  = messageLine "I cannot find a `NoteSelection` type:\n\n285| noteSelection  : Model -> NoteSelection\n                               "
      message2  = messageFormatError "^^^^^^^^^^^^^"
      message3  = messageLine "\nThese names seem close though:\n\n    "
      message4  = messageFormatSuggestion "XNoteSelection"
      message5  = messageLine  "\n    "
      message6  = messageFormatSuggestion "StorageAction"
      message7  = messageLine "\n    "
      message8  = messageFormatSuggestion "SC.NoteIdVersion"
      message9  = messageLine "\n    "
      message10 = messageFormatSuggestion "Dom.Element"
      message11 = messageLine "\n\n"
      message12 = messageFormatHint  "Hint"
      message13 = messageLine ": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm."
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

      _regionStart = LineAndColumn 285 27
      _regionEnd   = LineAndColumn 285 40
      _region      = Region _regionStart _regionEnd

  in Problem "NAMING ERROR" _region messages
