{-# LANGUAGE ScopedTypeVariables #-}

module ElmCompilerError (processError) where

import Prelude hiding (FilePath)
import Model
import RenderModel
import Theme
import ColorMap (maybeColor)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)
import Control.Monad (when)

import qualified Data.List.NonEmpty   as N

processError :: RuntimeConfig -> CompilerError -> IO ()
processError RuntimeConfig { runtimeConfigColorMap = colorMap, runtimeConfigConfig = config } compilerError =
  let stats            = configStats config
      numErrors        = configNumErrors config
      problems         = compilerErrorToProblemsAtFileLocations colorMap compilerError
      errorDescription = CompilerErrorDescription problems
  in renderCompilerErrorDescription stats numErrors errorDescription


compilerErrorToProblemsAtFileLocations :: ColorMap -> CompilerError -> N.NonEmpty ProblemsAtFileLocation
compilerErrorToProblemsAtFileLocations cm (CompilerError nonEmptyErrors _) = (processCompilerErrors cm) =<< nonEmptyErrors


processCompilerErrors :: ColorMap -> Error -> N.NonEmpty ProblemsAtFileLocation
processCompilerErrors cm (Error filePath _ problems) = problemsAtFileLocation cm (FilePath filePath) <$> problems


problemsAtFileLocation :: ColorMap -> FilePath -> Problem -> ProblemsAtFileLocation
problemsAtFileLocation cm filePath (Problem title (Region (LineAndColumn start end) _) messages) =
  let problemDescriptions :: N.NonEmpty ProblemDescription = fmap (problemDescription cm) messages
  in ProblemsAtFileLocation (Title title) filePath (start, end) problemDescriptions


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
renderCompilerErrorDescription :: Stats -> NumberOfErrors -> CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription stats numErrors (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) (filterByRequested numErrors errorDescriptions)
  newLines 2
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors (N.length errorDescriptions)
    newLines 2


filterByRequested :: NumberOfErrors -> N.NonEmpty ProblemsAtFileLocation -> [ProblemsAtFileLocation]
filterByRequested AllErrors = N.toList
filterByRequested OneError  = N.take 1

