{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad (when, join)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict      as M
import qualified Data.List.NonEmpty   as N

import Data.List.NonEmpty (NonEmpty(..), (<|))


type ColorMap = M.Map T.Text Color

data RuntimeConfig =
  RuntimeConfig {
    runtimeConfigConfig :: Config
  , runtimeConfigColorNames :: M.Map T.Text Color
  }


sharpen :: Config -> IO ()
sharpen config = do
  let runtimeConfig = RuntimeConfig config colorNames
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = decodeInput content :: Either String CompilerOutput

        errorIO :: String -> IO ()
        errorIO = T.putStrLn . ("Parsing error: " <>) . T.pack

        successIO :: CompilerOutput -> IO ()
        successIO = simplePrinter runtimeConfig

    in either errorIO successIO resultE


-- Should this be in here? It seems dissimilar to everything else.
decodeInput :: T.Text -> Either String CompilerOutput
decodeInput content = eitherDecode (B.fromStrict $ T.encodeUtf8 content)


-- Should we use a Reader/T  to pass through the context (ColorMap + Config) ?
simplePrinter :: RuntimeConfig -> CompilerOutput -> IO ()
simplePrinter rc (CompilerOutput nonEmptyErrors errorType) = do
  let
    desc :: N.NonEmpty (NonEmpty ProblemsAtFileLocation)
    desc = processErrors <$> nonEmptyErrors

    processErrors :: Error -> N.NonEmpty ProblemsAtFileLocation
    processErrors (Error filePath _ problems) = problemsAtFileLocation filePath <$> problems

    problemsAtFileLocation ::T.Text -> Problem -> ProblemsAtFileLocation
    problemsAtFileLocation filePath (Problem title (Region (LineAndColumn start end) _) messages) =
      let problemDescriptions :: N.NonEmpty ProblemDescription = fmap problemDescription messages
      in ProblemsAtFileLocation title filePath (start, end) problemDescriptions

    -- Document how this record syntax works with `doBold`, `doUnderline` etc
    problemDescription ::  Message -> ProblemDescription
    problemDescription (MessageLine (MessageText messageText)) = ProblemDescription [] messageText
    problemDescription (MessageFormatting MessageFormat {messageformatBold = doBold, messageformatUnderline = doUnderline, messageformatColor = doColor, messageformatString = messageText}) =
      let colorNamesMap = runtimeConfigColorNames rc
          formatting =
            catMaybes [ boolToMaybe doBold BoldFormat, boolToMaybe doUnderline UnderlineFormat, ColourFormat  <$> (doColor >>= maybeColor colorNamesMap) ]
      in ProblemDescription formatting messageText

  simpleErrorDescriptionInterpretter rc . CompilerErrorDescription . join $ desc


simpleErrorDescriptionInterpretter :: RuntimeConfig -> CompilerErrorDescription ->  IO ()
simpleErrorDescriptionInterpretter RuntimeConfig { runtimeConfigConfig = config }  (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) (filterByRequested (configNumErrors config) errorDescriptions)
  newLines 2
  when (configStats config == StatsOn) $ do
    printNumberOfCompilationErrors (N.length errorDescriptions)
    newLines 2


printNumberOfCompilationErrors :: Int -> IO ()
printNumberOfCompilationErrors errors =
  T.putStr "Compilation errors: " >> withColourInline (showt errors) Red


filterByRequested :: NumberOfErrors -> N.NonEmpty ProblemsAtFileLocation -> [ProblemsAtFileLocation]
filterByRequested AllErrors = N.toList
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
  let coords = showt s <> ":" <> showt e
      singleFileMessage = "-- " <> title <> " ---------- " <> filePath <> ":" <> coords
  in withColourInline singleFileMessage Cyan >> newLines 1


renderProblem :: ProblemDescription -> IO ()
renderProblem (ProblemDescription formatting message) =
  let formattingApplied = traverse_ renderFormatting formatting
  in formattingApplied >> T.putStr message >> resetAnsi


renderFormatting :: MessageFormatType -> IO ()
renderFormatting (ColourFormat color) = setSGR [SetColor Foreground Dull color]
renderFormatting UnderlineFormat      = setSGR [SetUnderlining SingleUnderline]
renderFormatting BoldFormat           = setSGR [SetConsoleIntensity BoldIntensity]


resetAnsi :: IO ()
resetAnsi = setSGR [Reset]


-- TODO: Do we need errorName ?



-- SUPPORT FUNCTIONS --


showt :: Show a => a -> T.Text
showt = T.pack . show


colorNames :: M.Map T.Text Color
colorNames =
  let allColors     = enumFromTo (minBound::Color) (maxBound:: Color)
      colorNamePair = (\color -> (T.toUpper . showt $ color, color)) <$> allColors
  in M.fromList colorNamePair


maybeColor :: M.Map T.Text Color -> T.Text -> Maybe Color
maybeColor colorNamesMap text =
  let upperText     = T.toUpper text
  in M.lookup upperText colorNamesMap


boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe predicate value = if predicate then Just value else Nothing


withColourInline :: T.Text -> Color -> IO ()
withColourInline text color = do
  setSGR [SetColor Foreground Dull color]
  T.putStr text
  resetAnsi
