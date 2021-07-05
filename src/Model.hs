{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import GHC.Generics
import System.Console.ANSI.Types
import Prelude hiding (FilePath)

import Data.Aeson.Casing (aesonPrefix, camelCase)
import Control.Applicative ((<|>))

import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.List.NonEmpty   as N
import qualified Data.Map.Strict      as M


-- TODO: Remove unused imports

type ColorMap = M.Map T.Text Color


data RuntimeConfig =
  RuntimeConfig {
    runtimeConfigConfig :: Config
  , runtimeConfigColorMap :: ColorMap
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
                       | OtherError DependencyError deriving stock (Generic, Eq, Show)


data DependencyError =
  DependencyError {
    dependencyerrorPath     :: T.Text
  , dependencyerrorTitle    :: T.Text
  , dependencyerrorMessage  :: N.NonEmpty Message
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
  parseJSON = \jsonValue ->
    let messageLineParser       :: Value -> Parser Message = \v -> fmap MessageLine (parseJSON v :: Parser MessageText)
        messageFormattingParser :: Value -> Parser Message = \v -> fmap MessageFormatting (parseJSON v :: Parser MessageFormat)
    in messageLineParser jsonValue <|> messageFormattingParser jsonValue


instance FromJSON ElmCompilerOutput where
  parseJSON = \jsonValue ->
    let elmErrorParser   :: Value -> Parser ElmCompilerOutput = \v -> ElmError <$>  (parseJSON v :: Parser CompilerError)
        otherErrorParser :: Value -> Parser ElmCompilerOutput = \v -> OtherError <$> (parseJSON v :: Parser DependencyError)
    in elmErrorParser jsonValue <|> otherErrorParser jsonValue


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


instance FromJSON DependencyError where
   parseJSON = genericParseJSON jsonOptions


data MessageFormatType = ColorFormat ColorType
                       | BoldFormat
                       | UnderlineFormat


data ColorType = CompilerErrorColor | CompilerSuggestionColor | DependencyErrorColor deriving stock (Eq, Show)

colorToMaybeColorType :: T.Text -> Maybe ColorType
colorToMaybeColorType colorText =
  case T.toUpper colorText of
    "RED"    -> Just CompilerErrorColor
    "YELLOW" -> Just CompilerSuggestionColor
    "GREEN"  -> Just DependencyErrorColor
    _        -> Nothing

-- TODO: Rename


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


decodeDependencyError :: T.Text -> Either String DependencyError
decodeDependencyError = decodeInput


decodeInput :: FromJSON a => T.Text -> Either String a
decodeInput = eitherDecode . B.fromStrict . T.encodeUtf8



-- SUPPORT FUNCTIONS --

-- Move to custom prelude
showt :: Show a => a -> T.Text
showt = T.pack . show


boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe predicate value = if predicate then Just value else Nothing
