{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<|>))

import GHC.Generics
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Aeson.Casing (aesonPrefix, camelCase)

sample1 :: B.ByteString
sample1 = "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"/Volumes/Work/projects/code/frontend/scrib-elm/src/StorageKeys.elm\",\"name\":\"StorageKeys\",\"problems\":[{\"title\":\"RESERVED WORD\",\"region\":{\"start\":{\"line\":78,\"column\":5},\"end\":{\"line\":78,\"column\":9}},\"message\":[\"It looks like you are trying to use `case` in this pattern:\n\n78|     case Save   ->  E.string \\\"save\\\"\n        \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^\"},\"\nThis is a reserved word! Try using some other name?\"]}]}]}"

data LineAndColumn = LineAndColumn { lineandcolumnLine :: Int, lineandcolumnColumn :: Int } deriving stock (Generic, Show)

data Region = Region { regionStart :: LineAndColumn, regionEnd :: LineAndColumn  } deriving stock (Generic, Show)

data Problem = Problem { problemTitle :: T.Text, problemRegion :: Region, problemMessage :: [Message] } deriving stock (Generic, Show)

data Error = Error  { errorPath :: T.Text, errorName :: T.Text, errorProblems :: [Problem] } deriving stock (Generic, Show)

data CompilerOutput = CompilerOutput { compileroutputErrors :: [Error],  compileroutputType :: T.Text }  deriving stock (Generic, Show)

data Message = MessageLine MessageText
             | MessageFormatting MessageFormat  deriving stock Show

newtype MessageText = MessageText T.Text deriving stock Show

data MessageFormat =
  MessageFormat {
    messageformatBold :: Bool
  , messageformatUnderline :: Bool
  , messageformatColor :: T.Text
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
      <*> v .: "color"
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


simplePrinter :: CompilerOutput -> T.Text
simplePrinter (CompilerOutput errors errorType) =
  case errors of
    [] -> ""
    xs -> T.intercalate "\n" (printError <$> xs)

printError :: Error -> T.Text
printError (Error filePath errorName problems) =
  T.concat $ fmap (\p -> errorName <> ":" <> filePath <> ":" <> printProblem p) problems


printProblem :: Problem -> T.Text
printProblem (Problem _ (Region (LineAndColumn start end) _) _) = (T.pack . show $ start) <> ":" <> (T.pack . show $ end)

someFunc :: IO ()
someFunc =
  let resultE = eitherDecode sample1 :: Either String CompilerOutput
  in T.putStrLn $ either (("Parsing error: " <>) . T.pack) simplePrinter resultE


