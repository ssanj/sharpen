{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( sharpen
    ) where

import System.IO
import Model
import ColorMap

import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)
import Data.Aeson (eitherDecode)
import Control.Monad (when, join)

import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as T
import qualified Data.Map.Strict      as M
import qualified Data.List.NonEmpty   as N

import Data.List.NonEmpty (NonEmpty(..), (<|))


sharpen :: Config -> IO ()
sharpen config = do
  let runtimeConfig = RuntimeConfig config allColorNamesMap
  content <- T.getContents
  if T.null content then T.putStrLn "Success!"
  else
    let resultE = decodeInput content :: Either String CompilerOutput

        errorIO :: String -> IO ()
        errorIO = T.putStrLn . ("Parsing error: " <>) . T.pack

        successIO :: CompilerOutput -> IO ()
        successIO = simplePrinter runtimeConfig

    in either errorIO successIO resultE


simplePrinter :: RuntimeConfig -> CompilerOutput -> IO ()
simplePrinter rc (CompilerOutput nonEmptyErrors errorType) = do
  let
    desc :: N.NonEmpty ProblemsAtFileLocation
    desc = processErrors =<< nonEmptyErrors

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

  simpleErrorDescriptionInterpretter rc $ CompilerErrorDescription desc


simpleErrorDescriptionInterpretter :: RuntimeConfig -> CompilerErrorDescription ->  IO ()
simpleErrorDescriptionInterpretter RuntimeConfig { runtimeConfigConfig = config }  (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) (filterByRequested (configNumErrors config) errorDescriptions)
  newLines 2
  when (configStats config == StatsOn) $ do
    printNumberOfCompilationErrors (N.length errorDescriptions)
    newLines 2


printNumberOfCompilationErrors :: Int -> IO ()
printNumberOfCompilationErrors errors =
  T.putStr "Compilation errors: " >> withColourInline (showt errors) errorColor


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
  in withColourInline singleFileMessage titleColor >> newLines 1


renderProblem :: ProblemDescription -> IO ()
renderProblem (ProblemDescription formatting message) =
  let formattingApplied = traverse_ renderFormatting formatting
  in formattingApplied >> T.putStr message >> resetAnsi
