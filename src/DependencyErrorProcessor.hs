module DependencyErrorProcessor (processError) where

import Model
import RenderModel
import Theme

import ColorMap (maybeColor)
import Data.Maybe (catMaybes)

processError :: RuntimeConfig -> DependencyError -> IO ()
processError RuntimeConfig { runtimeConfigColorMap = colorMap } (DependencyError path title nonEmptyMessages) =
  let problems = problemDescription colorMap <$> nonEmptyMessages
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
