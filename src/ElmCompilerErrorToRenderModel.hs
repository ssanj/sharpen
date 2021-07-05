{-# LANGUAGE ScopedTypeVariables #-}

module ElmCompilerErrorToRenderModel (processError2) where

import Prelude hiding (FilePath)
import Model
import RenderModel
import Data.Maybe (catMaybes)

import qualified Data.List.NonEmpty as N

processError2 :: CompilerError -> N.NonEmpty ProblemsAtFileLocation
processError2 compilerError =
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


  in compilerErrorToProblemsAtFileLocations compilerError

