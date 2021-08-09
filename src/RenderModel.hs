{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module RenderModel where

import Prelude hiding (FilePath)
import Model

import System.Console.ANSI (Color)

import qualified Data.Text          as T
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

newtype Title  = Title T.Text deriving stock (Eq, Show)

newtype FilePath  = FilePath T.Text deriving stock (Eq, Show)

data DependencyErrorDescription =
  DependencyErrorDescription {
    dependencyErrorDescriptionTitle                   :: Title
  , dependencyErrorDescriptionPath                    :: FilePath
  , dependencyErrorDescriptionPathProblemDescriptions :: N.NonEmpty ProblemDescription
  }

data CompilerErrorTheme =
  CompilerErrorTheme {
    compilerErrorThemeTitleColor      :: Color
  , compilerErrorThemeErrorColor      :: Color
  , compilerErrorThemeSuggestionColor :: Color
  }

newtype DependencyErrorTheme =
  DependencyErrorTheme {
    dependencyErrorThemeErrorColor :: Color
  }


data RenderTheme =
  RenderTheme {
    renderThemeCompilerErrorTheme   :: CompilerErrorTheme
  , renderThemeDependencyErrorTheme :: DependencyErrorTheme
  , renderThemeborder               :: [T.Text]
  , renderThemeparagraph            :: [T.Text]
  , renderThemegetColor             :: ColorType -> Color
}


newtype CompilerErrorDescription = CompilerErrorDescription (N.NonEmpty ProblemsAtFileLocation)

data CompilerErrorRenderModel =
  CompilerErrorRenderModel {
    compilerErrorRenderModelCompilerErrorDescription :: CompilerErrorDescription
  , compilerErrorRenderModelProblemsToDisplay        :: Int
  , compilerErrorRenderModelStats                    :: Stats
  }

