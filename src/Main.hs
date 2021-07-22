module Main where
import InputLexer 
import Parsers
import Text.Parsec
import System.Environment

main :: IO ()
main = do
  [args] <- getArgs
  file <- readFile args
  --print $ parse expressionParser "" "2;"
  --print $ parse returnParser "" "return 2;"
  --print $ parse expressionParser "" ""
  --print $ parse expressionParser "" "return 2;}"
  --print $ parseInput "int main() {return 2; }"
  --print file

  -- print $ parse functionParser "" "int main() { return 2 + 2; }"
  print $ parseInput file
