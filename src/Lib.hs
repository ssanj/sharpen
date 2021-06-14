{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( sharpen
    ) where

import System.IO hiding (FilePath)
import Prelude hiding (FilePath)

import Model
import ColorMap
import Theme

import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)
import Data.Aeson (eitherDecode)
import Control.Monad (when, join)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.Map.Strict      as M
import qualified Data.List.NonEmpty   as N

import Data.List.NonEmpty (NonEmpty(..), (<|))


sharpen :: Config -> IO ()
sharpen config = do
  let runtimeConfig = RuntimeConfig config allColorNamesMap
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = decodeElmCompilerOutput content :: Either String ElmCompilerOutput

        errorIO :: String -> IO ()
        errorIO = T.putStrLn . ("Parsing error: " <>) . T.pack

        successIO :: ElmCompilerOutput -> IO ()
        successIO = simplePrinter runtimeConfig

    in either errorIO successIO resultE

-- TODO: Pull out compiler error and generic error handling into separate files
-- TODO: Find a way to share common code

simplePrinter :: RuntimeConfig -> ElmCompilerOutput -> IO ()
simplePrinter rc elmCompilerOutput = do
  let
    compilerErrorToProblemsAtFileLocations :: CompilerError -> N.NonEmpty ProblemsAtFileLocation
    compilerErrorToProblemsAtFileLocations (CompilerError nonEmptyErrors _) = processCompilerErrors =<< nonEmptyErrors

    processCompilerErrors :: Error -> N.NonEmpty ProblemsAtFileLocation
    processCompilerErrors (Error filePath _ problems) = problemsAtFileLocation (FilePath filePath) <$> problems

    generalErrorToProblemsInFile ::  GeneralError -> GeneralProblemsInFile
    generalErrorToProblemsInFile (GeneralError path title nonEmptyMessages) =
      let problems = problemDescription <$> nonEmptyMessages
      in GeneralProblemsInFile (Title title) (FilePath path) problems


    problemsAtFileLocation :: FilePath -> Problem -> ProblemsAtFileLocation
    problemsAtFileLocation filePath (Problem title (Region (LineAndColumn start end) _) messages) =
      let problemDescriptions :: N.NonEmpty ProblemDescription = fmap problemDescription messages
      in ProblemsAtFileLocation (Title title) filePath (start, end) problemDescriptions

    -- Document how this record syntax works with `doBold`, `doUnderline` etc
    problemDescription ::  Message -> ProblemDescription
    problemDescription (MessageLine (MessageText messageText)) = ProblemDescription [] messageText
    problemDescription (MessageFormatting MessageFormat {messageformatBold = doBold, messageformatUnderline = doUnderline, messageformatColor = doColor, messageformatString = messageText}) =
      let colorNamesMap = runtimeConfigColorNames rc
          formatting =
            catMaybes [ boolToMaybe doBold BoldFormat, boolToMaybe doUnderline UnderlineFormat, ColourFormat  <$> (doColor >>= maybeColor colorNamesMap) ]
      in ProblemDescription formatting messageText

  case elmCompilerOutput of
    ElmError compilerError  -> simpleErrorDescriptionInterpreter rc $ CompilerErrorDescription $ compilerErrorToProblemsAtFileLocations compilerError
    OtherError generalError -> simpleGeneralErrorInterpreter rc $ generalErrorToProblemsInFile generalError


simpleGeneralErrorInterpreter :: RuntimeConfig -> GeneralProblemsInFile  -> IO ()
simpleGeneralErrorInterpreter RuntimeConfig { runtimeConfigConfig = config } probsInFile =
  do
    border
    renderGeneralErrorHeader config probsInFile
    paragraph
    traverse_  renderProblem $ generalProblemsInFilePathProblemDescriptions probsInFile
    border


renderGeneralErrorHeader :: Config -> GeneralProblemsInFile -> IO ()
renderGeneralErrorHeader _ (GeneralProblemsInFile title path _) =
  let heading  = "-- " <> showt title <> " ---------- " <> showt path
  in withColourInline heading titleColor >> newLines 1


simpleErrorDescriptionInterpreter :: RuntimeConfig -> CompilerErrorDescription ->  IO ()
simpleErrorDescriptionInterpreter RuntimeConfig { runtimeConfigConfig = config }  (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) (filterByRequested (configNumErrors config) errorDescriptions)
  newLines 2
  when (configStats config == StatsOn) $ do
    printNumberOfCompilationErrors (N.length errorDescriptions)
    newLines 2



filterByRequested :: NumberOfErrors -> N.NonEmpty ProblemsAtFileLocation -> [ProblemsAtFileLocation]
filterByRequested AllErrors = N.toList
filterByRequested OneError  = N.take 1

