{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<|>))

import GHC.Generics
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Aeson.Casing (aesonPrefix, camelCase)

import System.IO
import Control.DeepSeq

sample1 :: B.ByteString
sample1 = "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"/Volumes/Work/projects/code/frontend/scrib-elm/src/StorageKeys.elm\",\"name\":\"StorageKeys\",\"problems\":[{\"title\":\"RESERVED WORD\",\"region\":{\"start\":{\"line\":78,\"column\":5},\"end\":{\"line\":78,\"column\":9}},\"message\":[\"It looks like you are trying to use `case` in this pattern:\n\n78|     case Save   ->  E.string \\\"save\\\"\n        \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^\"},\"\nThis is a reserved word! Try using some other name?\"]}]}]}"

sample4 :: B.ByteString
sample4 = "{  \"type\": \"compile-errors\",  \"errors\": [    {      \"path\": \"/Volumes/Work/projects/code/frontend/scrib-elm/src/View.elm\",      \"name\": \"View\",      \"problems\": [        {          \"title\": \"NAMING ERROR\",          \"region\": {            \"start\": {              \"line\": 133,              \"column\": 98            },            \"end\": {              \"line\": 133,              \"column\": 106            }          },          \"message\": [            \"I cannot find a `Notxhing` variant:\n\n133|     NotesRefreshed              -> performOrGotoConfig model ({ model | notes = Loading, query = Notxhing }, getTopRemoteNotes)\n                                                                                                      \",            {              \"bold\": false,              \"underline\": false,              \"color\": \"RED\",              \"string\": \"^^^^^^^^\"            },            \"\nThese names seem close though:\n\n    \",            {              \"bold\": false,              \"underline\": false,              \"color\": \"yellow\",              \"string\": \"Nothing\"            },            \"\n    \",            {              \"bold\": false,              \"underline\": false,              \"color\": \"yellow\",              \"string\": \"Loading\"            },            \"\n    \",            {              \"bold\": false,              \"underline\": false,              \"color\": \"yellow\",              \"string\": \"NotAsked\"            },            \"\n    \",            {              \"bold\": false,              \"underline\": false,              \"color\": \"yellow\",              \"string\": \"NoteEdited\"            },            \"\n\n\",            {              \"bold\": false,              \"underline\": true,              \"color\": \"null\",              \"string\": \"Hint\"            },            \": Read <https://elm-lang.org/0.19.1/imports> to see how `import`\ndeclarations work in Elm.\"          ]        }      ]    }  ]}"

sample5 :: B.ByteString
sample5 = "{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"/Volumes/Work/projects/code/frontend/scrib-elm/src/Ports.elm\",\"name\":\"Ports\",\"problems\":[{\"title\":\"NAME CLASH\",\"region\":{\"start\":{\"line\":142,\"column\":1},\"end\":{\"line\":142,\"column\":38}},\"message\":[\"This file has multiple `encodePortAndPayloadWithStorageAccess` declarations. One\nhere:\n\n64| encodePortAndPayloadWithStorageAccess (PortTypeName portType) storageValue encoder =\n    \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nAnd another one here:\n\n142| encodePortAndPayloadWithStorageAccess (PortTypeName portType) storageArea key payloadEncoder payload =\n     \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nHow can I know which one you want? Rename one of them!\"]}]},{\"path\":\"/Volumes/Work/projects/code/frontend/scrib-elm/src/Save.elm\",\"name\":\"Save\",\"problems\":[{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":108,\"column\":116},\"end\":{\"line\":108,\"column\":186}},\"message\":[\"The 1st argument to `NoteWithId` is not what I expect:\n\n108|               (HavingContent (NoteWithId { noteId, noteVersion })) -> { model | note = HavingContent <| NoteWithId { noteId = noteId, noteVersion = noteVersion, noteText = newNoteText }, noteContentStatus  = NeedsToSave }\n                                                                                                                        \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nThis argument is a record of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ noteId : a, noteText : String, noteVersion : b }\"},\"\n\nBut `NoteWithId` needs the 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":108,\"column\":42},\"end\":{\"line\":108,\"column\":65}},\"message\":[\"The 1st argument to `NoteWithId` is weird.\n\n108|               (HavingContent (NoteWithId { noteId, noteVersion })) -> { model | note = HavingContent <| NoteWithId { noteId = noteId, noteVersion = noteVersion, noteText = newNoteText }, noteContentStatus  = NeedsToSave }\n                                              \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nIt is trying to match record values of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ c | noteId : a, noteVersion : b }\"},\"\n\nBut `NoteWithId` needs its 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":313,\"column\":68},\"end\":{\"line\":313,\"column\":135}},\"message\":[\"The 1st argument to `encodeNote` is not what I expect:\n\n313|     (NoteWithId { noteId, noteText, noteVersion }) -> N.encodeNote { noteId = noteId, noteText = noteText, noteVersion = noteVersion }\n                                                                        \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nThis argument is a record of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ noteId : a, noteText : b, noteVersion : c }\"},\"\n\nBut `encodeNote` needs the 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":313,\"column\":17},\"end\":{\"line\":313,\"column\":50}},\"message\":[\"The 1st argument to `NoteWithId` is weird.\n\n313|     (NoteWithId { noteId, noteText, noteVersion }) -> N.encodeNote { noteId = noteId, noteText = noteText, noteVersion = noteVersion }\n                     \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nIt is trying to match record values of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ d | noteId : a, noteText : b, noteVersion : c }\"},\"\n\nBut `NoteWithId` needs its 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":355,\"column\":32},\"end\":{\"line\":355,\"column\":42}},\"message\":[\"The 1st argument to `NoteWithId` is weird.\n\n355|     (HavingContent (NoteWithId { noteId }))  -> Just noteId\n                                    \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^\"},\"\nIt is trying to match record values of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ b | noteId : a }\"},\"\n\nBut `NoteWithId` needs its 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":362,\"column\":32},\"end\":{\"line\":362,\"column\":44}},\"message\":[\"The 1st argument to `NoteWithId` is weird.\n\n362|     (HavingContent (NoteWithId { noteText })) -> noteText\n                                    \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^\"},\"\nIt is trying to match record values of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ b | noteText : a }\"},\"\n\nBut `NoteWithId` needs its 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":348,\"column\":32},\"end\":{\"line\":348,\"column\":47}},\"message\":[\"The 1st argument to `NoteWithId` is weird.\n\n348|     (HavingContent (NoteWithId { noteVersion })) -> Just noteVersion\n                                    \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^\"},\"\nIt is trying to match record values of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ b | noteVersion : a }\"},\"\n\nBut `NoteWithId` needs its 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":409,\"column\":32},\"end\":{\"line\":409,\"column\":44}},\"message\":[\"The 1st argument to `NoteWithId` is weird.\n\n409|     (HavingContent (NoteWithId { noteText } )) -> not (String.isEmpty noteText)\n                                    \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^\"},\"\nIt is trying to match record values of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ b | noteText : a }\"},\"\n\nBut `NoteWithId` needs its 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":394,\"column\":83},\"end\":{\"line\":394,\"column\":151}},\"message\":[\"The 1st argument to `NoteWithId` is not what I expect:\n\n394|     (Just noteId, Just noteText, Just noteVersion) -> HavingContent <| NoteWithId { noteId = noteId,  noteText = noteText, noteVersion = noteVersion }\n                                                                                       \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nThis argument is a record of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ noteId : Int, noteText : String, noteVersion : Int }\"},\"\n\nBut `NoteWithId` needs the 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]},{\"title\":\"TYPE MISMATCH\",\"region\":{\"start\":{\"line\":132,\"column\":78},\"end\":{\"line\":132,\"column\":145}},\"message\":[\"The 1st argument to `NoteWithId` is not what I expect:\n\n132|     (NoteWithoutId noteText, Success { noteId, noteVersion }) -> NoteWithId  { noteId = noteId, noteText = noteText, noteVersion = noteVersion }\n                                                                                  \",{\"bold\":false,\"underline\":false,\"color\":\"RED\",\"string\":\"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\"},\"\nThis argument is a record of type:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"{ noteId : Int, noteText : String, noteVersion : Int }\"},\"\n\nBut `NoteWithId` needs the 1st argument to be:\n\n    \",{\"bold\":false,\"underline\":false,\"color\":\"yellow\",\"string\":\"N.Note\"},\"\"]}]}]}"

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

simplePrinter :: CompilerOutput -> T.Text
simplePrinter (CompilerOutput errors errorType) =
  case errors of
    [] -> ""
    xs -> T.intercalate "\n" (printError <$> xs)

printError :: Error -> T.Text
printError (Error filePath _ problems) =
  T.concat $ fmap (("\n\n" <>) . printProblem filePath) (take 1 problems)

printProblem :: T.Text -> Problem -> T.Text
printProblem filePath (Problem title (Region (LineAndColumn start end) _) messages) =
  let startAndEndCoords = (T.pack . show $ start) <> ":" <> (T.pack . show $ end)
      errorMessages = foldl (\a v -> a <> (printMessage v)) "" messages
  in  title <> ":" <> filePath <> ":" <>  startAndEndCoords <> "\n" <> errorMessages

printMessage :: Message -> T.Text
printMessage (MessageLine (MessageText messageText)) = messageText
printMessage (MessageFormatting (MessageFormat {messageformatString = messageText})) = messageText

someFunc :: IO ()
someFunc = do
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = eitherDecode (B.fromStrict $ T.encodeUtf8 content) :: Either String CompilerOutput
    in T.putStrLn $ either (("Parsing error: " <>) . T.pack) simplePrinter resultE
