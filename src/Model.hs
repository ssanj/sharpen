{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import GHC.Generics
import System.Console.ANSI.Types

import Data.Aeson.Casing (aesonPrefix, camelCase)
import Control.Applicative ((<|>))

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.List.NonEmpty   as N


data LineAndColumn =
  LineAndColumn {
    lineandcolumnLine :: Int
  , lineandcolumnColumn :: Int
  } deriving stock (Generic, Show)

data Region =
  Region {
    regionStart :: LineAndColumn
  , regionEnd :: LineAndColumn
  } deriving stock (Generic, Show)

data Problem =
  Problem {
    problemTitle :: T.Text
  , problemRegion :: Region
  , problemMessage :: N.NonEmpty Message
  } deriving stock (Generic, Show)

-- Problems in a single file
data Error =
  Error  {
    errorPath :: T.Text
  , errorName :: T.Text
  , errorProblems :: N.NonEmpty Problem
  } deriving stock (Generic, Show)

data CompilerOutput =
  CompilerOutput {
    compileroutputErrors :: N.NonEmpty Error
  , compileroutputType :: T.Text
  } deriving stock (Generic, Show)

data Message = MessageLine MessageText
             | MessageFormatting MessageFormat  deriving stock Show

newtype MessageText = MessageText T.Text deriving stock Show

data MessageFormat =
  MessageFormat {
    messageformatBold :: Bool
  , messageformatUnderline :: Bool
  , messageformatColor :: Maybe T.Text
  , messageformatString :: T.Text
  }  deriving stock (Generic, Show)

jsonOptions :: Options
jsonOptions = aesonPrefix camelCase

instance FromJSON Message where
  parseJSON = \v ->
    let messageLineParser       :: Value -> Parser Message = \v -> fmap MessageLine (parseJSON v :: Parser MessageText)
        messageFormattingParser :: Value -> Parser Message = \v -> fmap MessageFormatting (parseJSON v :: Parser MessageFormat)
    in (messageLineParser v) <|> (messageFormattingParser v)

instance FromJSON MessageText where
  parseJSON = withText "MessageText" (pure . MessageText)

instance FromJSON MessageFormat where
  parseJSON = withObject "MessageFormat" $ \v ->
    MessageFormat
      <$> v .: "bold"
      <*> v .: "underline"
      <*> v .:? "color"
      <*> v .: "string"

instance FromJSON LineAndColumn where
   parseJSON = genericParseJSON jsonOptions

instance FromJSON Region where
   parseJSON = genericParseJSON jsonOptions

instance FromJSON Problem where
   parseJSON = genericParseJSON jsonOptions

instance FromJSON Error where
   parseJSON = genericParseJSON jsonOptions

instance FromJSON CompilerOutput where
   parseJSON = genericParseJSON jsonOptions

data MessageFormatType = ColourFormat Color
                       | BoldFormat
                       | UnderlineFormat

-- TODO: Rename
data ProblemDescription = ProblemDescription [MessageFormatType] T.Text

-- Problems in a single file and single location
data ProblemsAtFileLocation =
  ProblemsAtFileLocation {
    problemsAtFileLocationTitle :: T.Text
  , problemsAtFileLocationFilePath :: T.Text
  , problemsAtFileLocationFileCoords :: (Int, Int)
  , problemsAtFileLocationProblemDescriptions :: N.NonEmpty ProblemDescription
  }

data CompilerErrorDescription = CompilerErrorDescription (N.NonEmpty ProblemsAtFileLocation)


data OutputDetail = Full
                | Succinct
                | Oneline deriving stock (Eq, Show)

-- We could have other constructors in future
-- | OnePerFile
-- | Other Int
data NumberOfErrors = AllErrors
                    | OneError

data Config =
  Config {
    numErrors :: NumberOfErrors
  , detail :: OutputDetail
  }

defaultConfig :: Config
defaultConfig = Config OneError Full