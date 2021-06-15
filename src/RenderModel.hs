{-# LANGUAGE ScopedTypeVariables #-}

module RenderModel where

import Prelude hiding (FilePath)
import Model

import Data.Maybe (catMaybes)
import ColorMap  (maybeColor)

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.List.NonEmpty as N

-- TODO: Remove unused imports
data ProblemDescription = ProblemDescription [MessageFormatType] T.Text


-- Problems in a single file and single location
data ProblemsAtFileLocation =
  ProblemsAtFileLocation {
    problemsAtFileLocationTitle               :: Title
  , problemsAtFileLocationFilePath            :: FilePath
  , problemsAtFileLocationFileCoords          :: (Int, Int)
  , problemsAtFileLocationProblemDescriptions :: N.NonEmpty ProblemDescription
  }

newtype Title  = Title T.Text deriving (Eq, Show)

newtype FilePath  = FilePath T.Text deriving (Eq, Show)

data GeneralProblemsInFile =
  GeneralProblemsInFile {
    generalProblemsInFileTitle                   :: Title
  , generalProblemsInFilePath                    :: FilePath
  , generalProblemsInFilePathProblemDescriptions :: N.NonEmpty ProblemDescription
  }


newtype CompilerErrorDescription = CompilerErrorDescription (N.NonEmpty ProblemsAtFileLocation)

