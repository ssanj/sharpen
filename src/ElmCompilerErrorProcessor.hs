{-# LANGUAGE ScopedTypeVariables #-}

module ElmCompilerErrorProcessor (processError) where

import Prelude hiding (FilePath)
import Model
import RenderModel
import Data.Maybe (catMaybes)

import qualified Data.List.NonEmpty as N

processError :: RuntimeConfig -> CompilerError -> CompilerErrorRenderModel
processError RuntimeConfig { runtimeConfigConfig = config } compilerError =
  let compilerErrorToProblemsAtFileLocations :: CompilerError -> N.NonEmpty ProblemsAtFileLocation
      compilerErrorToProblemsAtFileLocations (CompilerError nonEmptyErrors _) = processCompilerErrors =<< nonEmptyErrors


      processCompilerErrors :: Error -> N.NonEmpty ProblemsAtFileLocation
      processCompilerErrors (Error filePath _ compilerProblems) = problemsAtFileLocation (FilePath filePath) <$> compilerProblems


      problemsAtFileLocation :: FilePath -> Problem -> ProblemsAtFileLocation
      problemsAtFileLocation filePath (Problem title (Region (LineAndColumn start end) _) messages) =
        let problemDescriptions :: N.NonEmpty ProblemDescription = fmap problemDescription messages
        in ProblemsAtFileLocation (Title title) filePath (start, end) problemDescriptions

      problemDescription ::  Message -> ProblemDescription
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


      filterByRequested :: NumberOfErrors -> N.NonEmpty ProblemsAtFileLocation ->  N.NonEmpty ProblemsAtFileLocation
      filterByRequested AllErrors = id
      filterByRequested OneError  = pure . N.head

      stats                = configStats config
      numErrors            = configNumErrors config
      problems             = compilerErrorToProblemsAtFileLocations compilerError
      problemsToDisplay    = filterByRequested numErrors problems
      numProblemsDisplayed = N.length problemsToDisplay
      compilerErrorDesc    = CompilerErrorDescription problemsToDisplay

  in CompilerErrorRenderModel compilerErrorDesc numProblemsDisplayed stats
