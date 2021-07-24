module Main where
import Parsers
import Text.Parsec
import Text.Parsec.Char
import System.Environment
import GenCode

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

  --print $ parseInput "" "int main(){ 2 + 2 + 2; }"
  case parseInput file of
    Left e -> print e
    Right p -> print p >> let output = generate p in
      putStr output
      >> writeFile "basic.s" output
