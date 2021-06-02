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
    let decodedE = decodeInput input :: Either String CompilerOutput
    either failWithError assertCompilerOutput decodedE


failWithError :: String -> Assertion
failWithError e =
    let errorMessage = show e
    in assertFailure ("Could not decode input: " <> errorMessage)


assertCompilerOutput :: CompilerOutput -> Assertion
assertCompilerOutput co = do
    "compile-errors" @?= compileroutputType co
    let firstProblem = N.head . errorProblems . N.head . compileroutputErrors $ co
    firstProblem @?= namingError


getInput :: String -> IO T.Text
getInput = T.readFile


namingError :: Problem
namingError =
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
