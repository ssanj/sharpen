module Main where

import Lib (sharpen)
import Cmd

main :: IO ()
main = do
  args <- usingArgs
  case args of
    Left errorMessage    -> putStrLn errorMessage
    Right (Info message) -> putStrLn message
    Right (Run config)   -> sharpen config

