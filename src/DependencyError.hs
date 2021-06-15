{-# LANGUAGE OverloadedStrings #-}

module DependencyError (processError) where

import Model
import RenderModel
import Theme

import ColorMap (maybeColor)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)

processError :: RuntimeConfig -> GeneralError -> IO ()
processError RuntimeConfig { runtimeConfigColorMap = colorMap } (GeneralError path title nonEmptyMessages) =
  let problems = (problemDescription colorMap) <$> nonEmptyMessages
      genProblemsInFile = GeneralProblemsInFile (Title title) (FilePath path) problems
  in renderGeneralProblemsInFile genProblemsInFile


problemDescription ::  ColorMap -> Message -> ProblemDescription
problemDescription _ (MessageLine (MessageText messageText)) = ProblemDescription [] messageText
problemDescription colorNamesMap (MessageFormatting MessageFormat {messageformatBold = doBold, messageformatUnderline = doUnderline, messageformatColor = doColor, messageformatString = messageText}) =
  let formatting =
        catMaybes
          [
            boolToMaybe doBold BoldFormat
          , boolToMaybe doUnderline UnderlineFormat
          , ColourFormat  <$> (doColor >>= maybeColor colorNamesMap)
          ]
  in ProblemDescription formatting messageText


-- TODO: move this to Theme
renderGeneralProblemsInFile :: GeneralProblemsInFile  -> IO ()
renderGeneralProblemsInFile probsInFile =
  do
    border
    renderGeneralErrorHeader probsInFile
    paragraph
    traverse_  renderProblem $ generalProblemsInFilePathProblemDescriptions probsInFile
    border
