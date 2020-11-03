{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import System.Console.ANSI
import System.Console.ANSI.Types
import System.IO
import Control.DeepSeq
import Model

import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)
import Data.Aeson (eitherDecode)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict      as M
import qualified Data.List.NonEmpty   as N

import Data.List.NonEmpty (NonEmpty(..), (<|))


simplePrinter :: CompilerOutput -> IO ()
simplePrinter (CompilerOutput nonEmptyErrors errorType) =
  let desc = processErrors <$> nonEmptyErrors
  in simpleErrorDescriptionInterpretter $ CompilerErrorDescription $ desc >>= id

simpleErrorDescriptionInterpretter :: CompilerErrorDescription -> IO ()
simpleErrorDescriptionInterpretter (CompilerErrorDescription errorDescriptions) =
  do
    traverse_ (\ed -> newLines 2 >> renderFileProblems ed) errorDescriptions
    newLines 2

newLines :: Int -> IO ()
newLines n = traverse_ (const $ T.putStrLn "") [1..n]

renderFileProblems :: ProblemsAtFileLocation -> IO ()
renderFileProblems pfl@(ProblemsAtFileLocation title filePath (s, e) problems) =
  do
    createTitleAndFile pfl
    newLines 1
    traverse_  renderProblem problems

createTitleAndFile :: ProblemsAtFileLocation -> IO ()
createTitleAndFile (ProblemsAtFileLocation title filePath (s, e) _) =
  let coords = (showt s) <> ":" <> (showt e)
      singleFileMessage = "-- " <> title <> " ---------- " <> filePath <> ":" <> coords
  in withColour singleFileMessage Cyan

renderProblem :: ProblemDescription -> IO ()
renderProblem (ProblemDescription formatting message) =
  let formattingApplied = traverse_ renderFormatting formatting
  in formattingApplied >> T.putStr message >> resetAnsi

renderFormatting :: MessageFormatType -> IO ()
renderFormatting (ColourFormat color) = setSGR [(SetColor Foreground Dull color)]
renderFormatting UnderlineFormat      = setSGR [(SetUnderlining SingleUnderline)]
renderFormatting BoldFormat           = setSGR [(SetConsoleIntensity BoldIntensity)]

resetAnsi :: IO ()
resetAnsi = setSGR [Reset]

-- TODO: Do we need errorName ?
processErrors :: Error -> N.NonEmpty ProblemsAtFileLocation
processErrors (Error filePath _ problems) = (problemsAtFileLocation filePath) <$> problems

problemsAtFileLocation :: T.Text -> Problem -> ProblemsAtFileLocation
problemsAtFileLocation filePath (Problem title (Region (LineAndColumn start end) _) messages) =
  let problemDescriptions :: N.NonEmpty ProblemDescription = fmap problemDescription messages
  in ProblemsAtFileLocation title filePath (start, end) problemDescriptions

problemDescription :: Message -> ProblemDescription
problemDescription (MessageLine (MessageText messageText)) = ProblemDescription [] messageText
problemDescription (MessageFormatting (MessageFormat {messageformatBold = doBold, messageformatUnderline = doUnderline, messageformatColor = doColor, messageformatString = messageText})) =
  let formatting =
        catMaybes $ [ boolToMaybe doBold BoldFormat, boolToMaybe doUnderline UnderlineFormat, ColourFormat  <$> (doColor >>= maybeColor) ]
  in ProblemDescription formatting messageText

someFunc :: IO ()
someFunc = do
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = eitherDecode (B.fromStrict $ T.encodeUtf8 content) :: Either String CompilerOutput
    in either (T.putStrLn . ("Parsing error: " <>) . T.pack) simplePrinter resultE


-- SUPPORT FUNCTIONS --


showt :: Show a => a -> T.Text
showt = T.pack . show

colorNames :: M.Map T.Text Color
colorNames =
  let allColors     = enumFromTo (minBound::Color) (maxBound:: Color)
      colorNamePair = (\color -> (T.toUpper . showt $ color, color)) <$> allColors
  in M.fromList colorNamePair


maybeColor :: T.Text -> Maybe Color
maybeColor text =
  let upperText     = T.toUpper text
      colorNamesMap = colorNames
  in M.lookup upperText colorNamesMap

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe predicate value = if predicate then Just value else Nothing

withColour :: T.Text -> Color -> IO ()
withColour text color = do
  setSGR [(SetColor Foreground Dull color)]
  T.putStrLn text
  resetAnsi
