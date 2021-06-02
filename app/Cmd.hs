module Cmd
       (
          -- Data types
          RunOrInfo(..)

          -- Functions
       ,  usingArgs
       ) where


import Model              (defaultConfig, OutputDetail(..), Config(..), NumberOfErrors(..), Stats(..))
import System.Environment (getArgs)
import Data.List          (intercalate)


data RunOrInfo = Info String
               | Run Config


usingArgs :: IO (Either String RunOrInfo)
usingArgs = processArgs <$> getArgs


processArgs :: [String] -> Either String RunOrInfo
processArgs = extractConfig defaultConfig


-- TODO: Can we use the fact that we "know" args are not empty?
extractConfig :: Config -> [String] -> Either String RunOrInfo
extractConfig c args =
  case args of
    ("--detail" : detailOption : other) ->
      case detail detailOption of
        Right detailResult -> extractConfig (c { configDetail = detailResult }) other
        (Left x)           -> Left x

    ("--errors" : errorOption : other) ->
      case errors errorOption of
        Right errorsResult -> extractConfig (c { configNumErrors = errorsResult }) other
        (Left x)           -> Left x

    ("--stats" : statsOption : other) ->
      case stats statsOption of
        Right statsResult -> extractConfig (c { configStats = statsResult }) other
        (Left x)          -> Left x

    ["--help"]    -> Right $ Info usageString

    ["--version"] -> Right $ Info "version 0.1.0.0"

    []    -> Right $ Run c


    other -> Left usageString


usageString :: String
usageString =
  "Usage: sharpen --detail [full|short|oneline] --errors [all|one] --stats [on|off]"         <>
  "\n"                                                                                       <>
  "\n"                                                                                       <>
  "detail"                                                                                   <>
    "        "                                                                               <>
    "The amount of detail to show. One of 'full', 'short' or 'oneline'. The default is full" <>
  "\n"                                                                                       <>
  "errors"                                                                                   <>
    "        "                                                                               <>
    "How many errors to show. One of 'all' or 'one'. The default is 'one'"                   <>
  "\n"                                                                                       <>
  "stats"                                                                                    <>
    "         "                                                                              <>
  "Show stats on number of errors. One of 'on' or 'off'. The default is 'off'"


errors :: String -> Either String NumberOfErrors
errors "all" = Right AllErrors
errors "one" = Right OneError
errors other = Left $ errorString "errors" other ["all", "one"]


detail :: String -> Either String OutputDetail
detail  "full"    = Right FullDetail
detail  "short"   = Right ShortDetail
detail  "oneline" = Right OnelineDetail
detail  other     = Left $ errorString "detail" other ["full", "short", "oneline"]


stats :: String -> Either String Stats
stats "on"  = Right StatsOn
stats "off" = Right StatsOff
stats other = Left $ errorString "stats" other ["on", "off"]


errorString :: String -> String -> [String] -> String
errorString paramName invalidOption validOptions =
  "Invalid " <> paramName <> " parameter: " <> invalidOption <> ", valid values are: [" <> (intercalate "|" validOptions)  <> "]"

-- sharpen --detail [full|short|oneline] --errors [all|one] --stats [on|off]