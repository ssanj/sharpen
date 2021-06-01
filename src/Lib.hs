{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( sharpen
    , decodeInput
    ) where

import System.Console.ANSI
import System.Console.ANSI.Types
import System.IO
import Model

import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)
import Data.Aeson (eitherDecode)
import Control.Monad (when)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict      as M
import qualified Data.List.NonEmpty   as N

import Data.List.NonEmpty (NonEmpty(..), (<|))


type Printer = Config -> CompilerOutput -> IO ()


sharpen :: Config -> IO ()
sharpen config = do
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = decodeInput content :: Either String CompilerOutput
    in either (T.putStrLn . ("Parsing error: " <>) . T.pack) (simplePrinter config) resultE


decodeInput :: T.Text -> Either String CompilerOutput
decodeInput content = eitherDecode (B.fromStrict $ T.encodeUtf8 content)


simplePrinter :: Printer
simplePrinter config (CompilerOutput nonEmptyErrors errorType) =
  let desc = processErrors <$> nonEmptyErrors
  in simpleErrorDescriptionInterpretter config $ CompilerErrorDescription $ desc >>= id


simpleErrorDescriptionInterpretter :: Config -> CompilerErrorDescription -> IO ()
simpleErrorDescriptionInterpretter config (CompilerErrorDescription errorDescriptions) =
  do
    traverse_ (\ed -> newLines 2 >> renderFileProblems ed) (filterByRequested (configNumErrors config) errorDescriptions)
    newLines 2
    when (configStats config == StatsOn) $ do
      printNumberOfCompilationErrors (N.length errorDescriptions)
      newLines 2


printNumberOfCompilationErrors :: Int -> IO ()
printNumberOfCompilationErrors errors =
  T.putStr "Compilation errors: " >> withColourInline (showt errors) Red


filterByRequested :: NumberOfErrors -> N.NonEmpty ProblemsAtFileLocation -> [ProblemsAtFileLocation]
filterByRequested AllErrors = N.toList . id
filterByRequested OneError  = N.take 1


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
  in withColourInline singleFileMessage Cyan >> newLines 1


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


withColourInline :: T.Text -> Color -> IO ()
withColourInline text color = do
  setSGR [(SetColor Foreground Dull color)]
  T.putStr text
  resetAnsi
