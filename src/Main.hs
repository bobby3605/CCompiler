module Main where
import InputLexer 
import Parsers
import Text.Parsec
import System.Environment

main :: IO ()
main = do
  [args] <- getArgs
  file <- readFile args
  print $ lexInput file 

