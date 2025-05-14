module Main where

import Parser

-- Read input from stdin and feed it to the parser
main :: IO ()
main = do
  input <- getContents
  case compile input of
    Left err -> putStrLn $ "Error: " ++ err
    Right term -> putStrLn $ "Parsed: " ++ show term
