module Parsers where
import Text.Parsec
import Text.Parsec.Char
import Symbol
import Control.Monad

type Program = [Function]
data Function = Function { returnType :: String
                         ,name :: String
                         ,arguments :: [(String, String)]
                         ,body :: [Statement]
                         } deriving (Show)

data Statement = Return

data Expression = Integer

programParser :: Text.Parsec.Parsec String () Program
programParser = do
  many functionParser
  
functionParser :: Text.Parsec.Parsec String () Function
functionParser = do
  spaces <|> skipMany endOfLine
  returnType <- many1 alphaNum
  spaces <|> skipMany endOfLine
  name <- many1 alphaNum
  spaces <|> skipMany endOfLine
  char '('
  spaces <|> skipMany endOfLine
  arguments <- sepBy argumentParser (char ',')
  spaces <|> skipMany endOfLine
  char ')'
  spaces <|> skipMany endOfLine
  char '{'
  spaces <|> skipMany endOfLine
  body <- many1 statementParser
  spaces <|> skipMany endOfLine
  char '}'
  return $ Function { returnType = returnType
                  , name = name
                  , arguments = arguments
                  , body = body
                  }
  
  
argumentParser :: Text.Parsec.Parsec String () (String,String)
argumentParser = do
  spaces <|> skipMany endOfLine
  argumentType <- many1 alphaNum
  spaces <|> skipMany endOfLine
  argumentName <- many1 alphaNum
  return (argumentType,argumentName)

statementParser :: Text.Parsec.Parsec String () Statement
statementParser = do
  choice [ returnParser ]

returnParser :: Text.Parsec.Parsec String () String
returnParser = do
  one <- string "return"
  spaces <|> skipMany endOfLine
  two <- manyTill anyToken $ string ";"
  spaces <|> skipMany endOfLine
  return $ one ++ " " ++ two ++ ";"
    
