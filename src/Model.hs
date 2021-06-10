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

import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.List.NonEmpty   as N
import qualified Data.Map.Strict      as M


type ColorMap = M.Map T.Text Color


data RuntimeConfig =
  RuntimeConfig {
    runtimeConfigConfig :: Config
  , runtimeConfigColorNames :: ColorMap
  }


data LineAndColumn =
  LineAndColumn {
    lineandcolumnLine :: Int
  , lineandcolumnColumn :: Int
  } deriving stock (Generic, Eq, Show)


data Region =
  Region {
    regionStart :: LineAndColumn
  , regionEnd :: LineAndColumn
  } deriving stock (Generic, Eq, Show)


data Problem =
  Problem {
    problemTitle :: T.Text
  , problemRegion :: Region
  , problemMessage :: N.NonEmpty Message
  } deriving stock (Generic, Eq, Show)


-- Problems in a single file
data Error =
  Error  {
    errorPath :: T.Text
  , errorName :: T.Text
  , errorProblems :: N.NonEmpty Problem
  } deriving stock (Generic, Eq, Show)


data ElmCompilerOutput = ElmError CompilerError
                       | OtherError GeneralError deriving stock (Generic, Eq, Show)


data GeneralError =
  GeneralError {
    generalerrorPath     :: T.Text
  , generalerrorTitle    :: T.Text
  , generalerrorMessage  :: N.NonEmpty Message
  } deriving stock (Generic, Eq, Show)


data CompilerError =
  CompilerError {
    compilererrorErrors :: N.NonEmpty Error
  , compilererrorType :: T.Text
  } deriving stock (Generic, Eq, Show)


data Message = MessageLine MessageText
             | MessageFormatting MessageFormat  deriving stock (Show, Eq)


newtype MessageText = MessageText T.Text deriving stock (Show, Eq)


data MessageFormat =
  MessageFormat {
    messageformatBold :: Bool
  , messageformatUnderline :: Bool
  , messageformatColor :: Maybe T.Text
  , messageformatString :: T.Text
  }  deriving stock (Generic, Eq, Show)


jsonOptions :: Options
jsonOptions = aesonPrefix camelCase


instance FromJSON Message where
  parseJSON = \v ->
    let messageLineParser       :: Value -> Parser Message = \v -> fmap MessageLine (parseJSON v :: Parser MessageText)
        messageFormattingParser :: Value -> Parser Message = \v -> fmap MessageFormatting (parseJSON v :: Parser MessageFormat)
    in messageLineParser v <|> messageFormattingParser v


instance FromJSON ElmCompilerOutput where
  parseJSON = \v ->
    let elmErrorParser   :: Value -> Parser ElmCompilerOutput = \v -> ElmError <$>  (parseJSON v :: Parser CompilerError)
        otherErrorParser :: Value -> Parser ElmCompilerOutput = \v -> OtherError <$> (parseJSON v :: Parser GeneralError)
    in elmErrorParser v <|> otherErrorParser v


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


instance FromJSON CompilerError where
   parseJSON = genericParseJSON jsonOptions


instance FromJSON GeneralError where
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


newtype CompilerErrorDescription = CompilerErrorDescription (N.NonEmpty ProblemsAtFileLocation)


data OutputDetail = FullDetail
                  | ShortDetail
                  | OnelineDetail deriving stock (Eq, Show)


-- We could have other constructors in future
-- | OnePerFile
-- | Other Int
data NumberOfErrors = AllErrors
                    | OneError deriving stock (Eq, Show)


data Stats = StatsOn
           | StatsOff deriving stock (Eq, Show)


data Config =
  Config {
    configNumErrors :: NumberOfErrors
  , configDetail    :: OutputDetail
  , configStats     :: Stats
  }  deriving stock (Eq, Show)


defaultConfig :: Config
defaultConfig = Config OneError FullDetail StatsOff


decodeElmCompilerOutput :: T.Text -> Either String ElmCompilerOutput
decodeElmCompilerOutput = decodeInput


decodeCompilerError :: T.Text -> Either String CompilerError
decodeCompilerError = decodeInput


decodeGeneralError :: T.Text -> Either String GeneralError
decodeGeneralError = decodeInput


decodeInput :: FromJSON a => T.Text -> Either String a
decodeInput = eitherDecode . B.fromStrict . T.encodeUtf8


-- SUPPORT FUNCTIONS --


showt :: Show a => a -> T.Text
showt = T.pack . show


boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe predicate value = if predicate then Just value else Nothing
