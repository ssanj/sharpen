module Main where

import Lib (sharpen)
import Cmd
import Colors
import Model (RuntimeConfig(..))

main :: IO ()
main = do
  args <- usingArgs
  case args of
    Left errorMessage    -> putStrLn errorMessage
    Right (Info message) -> putStrLn message
    Right (Run config)   -> sharpen (RuntimeConfig defaultTheme config)

