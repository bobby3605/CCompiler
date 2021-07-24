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

data Bop =
    Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Show)

data Uop =
    Return
  | Negation
  | LogicalNegation
  | BitwiseComp
  deriving (Show)

data Token =
    Integer Integer
  | Double Double
  | Float Float
  | Char Char
  | String String
  deriving (Show)

data Expression =
    Bop Bop Expression Expression
  | Uop Uop Expression
  | Token Token
  deriving (Show)

precedenceOfOperator :: Expression -> Integer
precedenceOfOperator (Uop Negation _)             = 0
precedenceOfOperator (Uop BitwiseComp _)          = 0
precedenceOfOperator (Uop LogicalNegation _)      = 0
precedenceOfOperator (Token (Integer _))          = 1
precedenceOfOperator (Token (Double _))           = 1
precedenceOfOperator (Token (Float _))            = 1
precedenceOfOperator (Token (String _))           = 1
precedenceOfOperator (Token (Char _))             = 1
precedenceOfOperator (Bop Add _ _)                = 2
precedenceOfOperator (Bop Sub _ _)                = 2
precedenceOfOperator (Bop Mul _ _)                = 2
precedenceOfOperator (Bop Div _ _)                = 2
precedenceOfOperator (Bop And _ _)                = 3
precedenceOfOperator (Bop Or _ _)                 = 3
precedenceOfOperator (Bop Equal _ _)              = 3
precedenceOfOperator (Bop NotEqual _ _)           = 3
precedenceOfOperator (Bop LessThan _ _)           = 3
precedenceOfOperator (Bop LessThanOrEqual _ _)    = 3
precedenceOfOperator (Bop GreaterThan _ _)        = 3
precedenceOfOperator (Bop GreaterThanOrEqual _ _) = 3
precedenceOfOperator (Uop Return _)               = 10

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
  <|> try negationParser
  <|> try bitwiseComplementParser
  <|> try logicalNegationParser
  <|> try andParser
  <|> try orParser
  <|> try equalParser
  <|> try notEqualParser
  <|> try lessThanParser
  <|> try lessThanOrEqualParser
  <|> try greaterThanParser
  <|> try greaterThanOrEqualParser
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

andParser :: Text.Parsec.Parsec String () Expression
andParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "&&"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop And expr1 expr2

orParser :: Text.Parsec.Parsec String () Expression
orParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "||"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop Or expr1 expr2

equalParser :: Text.Parsec.Parsec String () Expression
equalParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "=="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop Equal expr1 expr2

notEqualParser :: Text.Parsec.Parsec String () Expression
notEqualParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "!="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop NotEqual expr1 expr2

lessThanParser :: Text.Parsec.Parsec String () Expression
lessThanParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "<"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop LessThan expr1 expr2

lessThanOrEqualParser :: Text.Parsec.Parsec String () Expression
lessThanOrEqualParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "<="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop LessThanOrEqual expr1 expr2

greaterThanParser :: Text.Parsec.Parsec String () Expression
greaterThanParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string ">"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop GreaterThan expr1 expr2

greaterThanOrEqualParser :: Text.Parsec.Parsec String () Expression
greaterThanOrEqualParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string ">="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ Bop GreaterThanOrEqual expr1 expr2

negationParser :: Text.Parsec.Parsec String () Expression
negationParser = do
  string "-"
  spaces <|> skipMany endOfLine
  expr <- try expressionParser <|> tokenParser
  return $ Uop Negation expr

bitwiseComplementParser :: Text.Parsec.Parsec String () Expression
bitwiseComplementParser = do
  string "~"
  spaces <|> skipMany endOfLine
  expr <- try expressionParser <|> tokenParser
  return $ Uop BitwiseComp expr

logicalNegationParser :: Text.Parsec.Parsec String () Expression
logicalNegationParser = do
  string "!"
  spaces <|> skipMany endOfLine
  expr <- try expressionParser <|> tokenParser
  return $ Uop LogicalNegation expr

parenthesesParser :: Text.Parsec.Parsec String () Expression
parenthesesParser = emptyParser

endParser :: Text.Parsec.Parsec String () Expression
endParser = do
  string "}"
  return $ Token $ String ""
  
emptyParser :: Text.Parsec.Parsec String () Expression
emptyParser = do
  notFollowedBy anyToken
  return $ Token $ String ""
  
returnParser :: Text.Parsec.Parsec String () Expression
returnParser = do
  string "return"
  spaces <|> skipMany endOfLine
  returnExpression <- expressionParser
  return $ Uop Return returnExpression

-- i don't think these respect PEMDAS
addParser :: Text.Parsec.Parsec String () Expression
addParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "+"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Add num1 num2)

-- if precedence of expressionParser output is greater than addParser, make addParser a sub expression of expressionParser output
-- Add num1 (And num2 (Add num3 num4))
-- And (Add num1 num2) (Add num3 num4)
-- expr1 from And becomes Expr2 of the first Add
-- The new first Add becomes expr1 of And
-- expr2 from And stays
-- Otherwise, make the Expression normally

operatorPrecedenceFix :: Expression -> Expression
operatorPrecedenceFix (Bop currOp num1 num2) =
  if precedenceOfOperator num2 > precedenceOfOperator (Bop currOp num1 num2) then
    case num2 of
      Bop nextOp expr1 expr2 -> Bop nextOp (Bop currOp num1 expr1) expr2
      a -> error $ "operatorPrecedenceFix error: " ++ show a
  else
    Bop currOp num1 num2

{--
operatorPrecedenceFix :: Expression -> Expression
operatorPrecedenceFix (currExpr num1 num2) =
  if precedenceOfOperator num2 > precedenceOfOperator (currExpr num1 num2) then
    case num2 of
      nextExpr expr1 expr2 -> nextExpr (currExpr num1 expr1) expr2
      a -> error $ "operatorPrecedenceFix error: " ++ show a
  else
    currExpr num1 num2
--}

subParser :: Text.Parsec.Parsec String () Expression
subParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "-"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Bop Sub num1 num2

mulParser :: Text.Parsec.Parsec String () Expression
mulParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "*"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Bop Mul num1 num2

divParser :: Text.Parsec.Parsec String () Expression
divParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "/"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ Bop Div num1 num2

intLiteralParser :: Text.Parsec.Parsec String () Expression
intLiteralParser = do
  -- many1 digit is consuming the input
  num <- many1 digit
  skipMany endOfLine
  -- notFollowedBy $ string "." <|> string "+"
  string " " <|> string ";"
  spaces <|> skipMany endOfLine
  return $  Token $ Integer (read num :: Integer)
  
intParser :: Text.Parsec.Parsec String () Expression
intParser = do
  string "int"
  spaces <|> skipMany endOfLine
  -- Needs a noneOf $ string "."
  num <- manyTill digit (string ";" <|> string "." <|> string " ")
  return $ Token $ Integer (read num :: Integer)

doubleParser :: Text.Parsec.Parsec String () Expression
doubleParser = do
  string "double"
  spaces <|> skipMany endOfLine
  num1 <- manyTill digit (string ".")
  num2 <- manyTill digit (string " " <|> string ";")
  return $ Token $ Double (read (num1 ++ "." ++ num2) :: Double)

floatParser :: Text.Parsec.Parsec String () Expression
floatParser = do
  string "float"
  spaces <|> skipMany endOfLine
  num1 <- manyTill digit (string ".")
  num2 <- manyTill digit (string " " <|> string ";")
  return $ Token $ Float (read (num1 ++ "." ++ num2) :: Float)

-- TODO: add support for escape characters
stringParser :: Text.Parsec.Parsec String () Expression
stringParser = do
  oneOf "\""
  s <- manyTill anyToken (string "\"")
  return $ Token $ String s

charParser :: Text.Parsec.Parsec String () Expression
charParser = do
  oneOf "'"
  c <- anyToken
  return $ Token $ Char c
