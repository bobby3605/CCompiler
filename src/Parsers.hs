module Parsers where
import Text.Parsec
import Text.Parsec.Char
import Control.Monad

type Program = [Function]
data Function = Function { returnType :: String
                         ,name :: String
                         ,arguments :: [(String, String)]
                         ,body :: [Expression]
                         } deriving (Show)

data Expression  =
    Integer Integer
  | Double Double
  | Float Float
  | String String
  | Char Char
  | Return Expression
  | Add Expression Expression 
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show)

parseInput :: String -> Either ParseError Program
parseInput = parse programParser ""

programParser :: Text.Parsec.Parsec String () Program
programParser = do
  many1 functionParser

functionParser :: Text.Parsec.Parsec String () Function
functionParser = do
  spaces <|> skipMany endOfLine
  returnType <- many1 alphaNum
  spaces <|> skipMany endOfLine
  name <- many1 alphaNum
  spaces <|> skipMany endOfLine
  string "("
  spaces <|> skipMany endOfLine
  arguments <- many argumentParser
  spaces <|> skipMany endOfLine
  string ")"
  spaces <|> skipMany endOfLine
  string "{"
  spaces <|> skipMany endOfLine
  body <- many expressionParser
  spaces <|> skipMany endOfLine
  string "}"
  spaces <|> skipMany endOfLine
  return $ Function { returnType = returnType
                    , name = name
                    , arguments = arguments
                    , body = body }

argumentParser :: Text.Parsec.Parsec String () (String,String)
argumentParser = do
  argType <- many1 alphaNum
  spaces <|> skipMany endOfLine
  argName <- many1 alphaNum
  return (argType, argName)

expressionParser :: Text.Parsec.Parsec String () Expression
expressionParser = do
  spaces <|> skipMany endOfLine
  notFollowedBy $ string "}"
  emptyParser
  <|> returnParser
  <|> try addParser
  <|> try subParser
  <|> try mulParser
  <|> try divParser
  <|> intParser
  <|> doubleParser
  <|> floatParser
  <|> stringParser
  <|> charParser
  <|> intLiteralParser

tokenParser :: Text.Parsec.Parsec String () Expression
tokenParser = do
  intLiteralParser
  -- <|> parenthesesParser

parenthesesParser :: Text.Parsec.Parsec String () Expression
parenthesesParser = emptyParser

endParser :: Text.Parsec.Parsec String () Expression
endParser = do
  string "}"
  return $ String ""
  
emptyParser :: Text.Parsec.Parsec String () Expression
emptyParser = do
  notFollowedBy anyToken
  return $ String ""
  
returnParser :: Text.Parsec.Parsec String () Expression
returnParser = do
  string "return"
  spaces <|> skipMany endOfLine
  returnExpression <- expressionParser
  return $ Return returnExpression

addParser :: Text.Parsec.Parsec String () Expression
addParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "+"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Add num1 num2

subParser :: Text.Parsec.Parsec String () Expression
subParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "-"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Sub num1 num2

mulParser :: Text.Parsec.Parsec String () Expression
mulParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "*"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Mul num1 num2

divParser :: Text.Parsec.Parsec String () Expression
divParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "/"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Div num1 num2

intLiteralParser :: Text.Parsec.Parsec String () Expression
intLiteralParser = do
  -- many1 digit is consuming the input
  num <- many1 digit
  skipMany endOfLine
  -- notFollowedBy $ string "." <|> string "+"
  string " " <|> string ";"
  spaces <|> skipMany endOfLine
  return $ Integer (read num :: Integer)
  
intParser :: Text.Parsec.Parsec String () Expression
intParser = do
  string "int"
  spaces <|> skipMany endOfLine
  -- Needs a noneOf $ string "."
  num <- manyTill digit (string ";" <|> string "." <|> string " ")
  return $ Integer (read num :: Integer)

doubleParser :: Text.Parsec.Parsec String () Expression
doubleParser = do
  string "double"
  spaces <|> skipMany endOfLine
  num1 <- manyTill digit (string ".")
  num2 <- manyTill digit (string " " <|> string ";")
  return $ Double (read (num1 ++ "." ++ num2) :: Double)

floatParser :: Text.Parsec.Parsec String () Expression
floatParser = do
  string "float"
  spaces <|> skipMany endOfLine
  num1 <- manyTill digit (string ".")
  num2 <- manyTill digit (string " " <|> string ";")
  return $ Float (read (num1 ++ "." ++ num2) :: Float)

-- TODO: add support for escape characters
stringParser :: Text.Parsec.Parsec String () Expression
stringParser = do
  oneOf "\""
  s <- manyTill anyToken (string "\"")
  return $ String s

charParser :: Text.Parsec.Parsec String () Expression
charParser = do
  oneOf "'"
  c <- anyToken
  return $ Char c
  
