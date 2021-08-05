module DependencyErrorToRenderModel (processError2) where

import Model
import RenderModel
import Theme

import Data.Maybe (catMaybes)

processError2 :: DependencyError -> IO ()
processError2 (DependencyError path title nonEmptyMessages) =
  let problems = problemDescription <$> nonEmptyMessages
      genProblemsInFile = DependencyErrorDescription (Title title) (FilePath path) problems
  in renderGeneralProblemsInFile genProblemsInFile


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
