module Parsers where
import Text.Parsec
import Text.Parsec.Char
import InputLexer
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


functionParser :: Text.Parsec.Parsec String () Function
functionParser = do
  spaces
  returnType <- many1 alphaNum
  spaces
  name <- many1 alphaNum
  spaces
  arguments <- many1 argumentParser
  spaces
  body <- many1 expressionParser
  return $ Function { returnType = returnType
                    , name = name
                    , arguments = arguments
                    , body = body }

argumentParser :: Text.Parsec.Parsec String () (String,String)
argumentParser = do
  argType <- many1 alphaNum
  spaces
  argName <- many1 alphaNum
  return (argType, argName)

expressionParser :: Text.Parsec.Parsec String () Expression
expressionParser = do
  choice [ intParser
         , doubleParser
         , floatParser
         , stringParser
         , charParser
         , returnParser
         , addParser
         , subParser
         , mulParser
         , divParser
         ]

returnParser :: Text.Parsec.Parsec String () Expression
returnParser = do
  string "return"
  returnExpression <- manyTill anyToken $ string ";"
  return $ Return (case parse expressionParser "" returnExpression of
                     Left e -> Char $ head $ show e-- This should probably throw an error
                     Right expr -> Return expr
                     )

addParser :: Text.Parsec.Parsec String () Expression
addParser = do
  num1 <- expressionParser
  spaces
  string "+"
  spaces
  num2 <- expressionParser
  return $ Add num1 num2

subParser :: Text.Parsec.Parsec String () Expression
subParser = do
  num1 <- expressionParser
  spaces
  string "-"
  spaces
  num2 <- expressionParser
  return $ Sub num1 num2

mulParser :: Text.Parsec.Parsec String () Expression
mulParser = do
  num1 <- expressionParser
  spaces
  string "*"
  spaces
  num2 <- expressionParser
  return $ Mul num1 num2

divParser :: Text.Parsec.Parsec String () Expression
divParser = do
  num1 <- expressionParser
  spaces
  string "/"
  spaces
  num2 <- expressionParser
  return $ Div num1 num2

intParser :: Text.Parsec.Parsec String () Expression
intParser = do
  string "int"
  spaces
  num <- manyTill digit (string ";" <|> string "." <|> string " ")
  return $ Integer (read num :: Integer)

doubleParser :: Text.Parsec.Parsec String () Expression
doubleParser = do
  string "double"
  spaces
  num1 <- manyTill digit (string ".")
  num2 <- manyTill digit (string " " <|> string ";")
  return $ Double (read (num1 ++ "." ++ num2) :: Double)

floatParser :: Text.Parsec.Parsec String () Expression
floatParser = do
  string "float"
  spaces
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
  
