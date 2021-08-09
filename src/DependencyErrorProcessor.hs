module DependencyErrorProcessor (processError) where

import Model
import RenderModel

import Data.Maybe (catMaybes)


processError :: RuntimeConfig -> DependencyError -> DependencyErrorDescription
processError _ (DependencyError path title nonEmptyMessages) =
  let problems = problemDescription <$> nonEmptyMessages
      genProblemsInFile = DependencyErrorDescription (Title title) (FilePath path) problems
  in genProblemsInFile


problemDescription :: Message -> ProblemDescription
problemDescription (MessageLine (MessageText messageText)) = ProblemDescription [] messageText
problemDescription (MessageFormatting MessageFormat {messageformatBold = doBold, messageformatUnderline = doUnderline, messageformatColor = doColor, messageformatString = messageText}) =
  let formatting =
        catMaybes
          [
            boolToMaybe doBold BoldFormat
          , boolToMaybe doUnderline UnderlineFormat
          , ColorFormat  <$> (doColor >>= colorToMaybeColorType)
          ]
  in ProblemDescription formatting messageText
