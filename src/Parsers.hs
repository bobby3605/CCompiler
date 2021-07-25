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
  | Modulo
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | BitwiseShiftLeft
  | BitwiseShiftRight
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
  | Variable String String
  deriving (Show)

data Expression =
    Bop Bop Expression Expression
  | Uop Uop Expression
  | Token Token
  | DeclareVariable String String (Maybe Expression)
  | AssignVariable String Expression
  deriving (Show)


precedenceOfOperator :: Expression -> Float
precedenceOfOperator (Uop Negation _)             = 0
precedenceOfOperator (Uop BitwiseComp _)          = 0
precedenceOfOperator (Uop LogicalNegation _)      = 0
precedenceOfOperator (Token (Integer _))          = 1
precedenceOfOperator (Token (Double _))           = 1
precedenceOfOperator (Token (Float _))            = 1
precedenceOfOperator (Token (String _))           = 1
precedenceOfOperator (Token (Char _))             = 1
precedenceOfOperator (Token (Variable _ _))       = 1
precedenceOfOperator (Bop Add _ _)                = 2
precedenceOfOperator (Bop Sub _ _)                = 2
precedenceOfOperator (Bop Mul _ _)                = 2.5
precedenceOfOperator (Bop Div _ _)                = 2.5
precedenceOfOperator (Bop And _ _)                = 3
precedenceOfOperator (Bop Or _ _)                 = 3
precedenceOfOperator (Bop Equal _ _)              = 3
precedenceOfOperator (Bop NotEqual _ _)           = 3
precedenceOfOperator (Bop LessThan _ _)           = 3
precedenceOfOperator (Bop LessThanOrEqual _ _)    = 3
precedenceOfOperator (Bop GreaterThan _ _)        = 3
precedenceOfOperator (Bop GreaterThanOrEqual _ _) = 3
precedenceOfOperator (Bop Modulo _ _)             = 4
precedenceOfOperator (Bop BitwiseAnd _ _)         = 4
precedenceOfOperator (Bop BitwiseOr _ _)          = 4
precedenceOfOperator (Bop BitwiseXor _ _)         = 4
precedenceOfOperator (Bop BitwiseShiftLeft _ _)   = 4
precedenceOfOperator (Bop BitwiseShiftRight _ _)  = 4
precedenceOfOperator DeclareVariable {}      = 9
precedenceOfOperator AssignVariable {}     = 9
precedenceOfOperator (Uop Return _)               = 10

parseInput :: String -> Either ParseError Program
parseInput = parse programParser ""

programParser :: Text.Parsec.Parsec String () Program
programParser =
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
  <|> try moduloParser
  <|> try bitwiseAndParser
  <|> try bitwiseOrParser
  <|> try bitwiseXorParser
  <|> try bitwiseShiftLeftParser
  <|> try bitwiseShiftRightParser
  <|> try variableParser
  <|> tokenParser

tokenParser :: Text.Parsec.Parsec String () Expression
tokenParser =
  try intLiteralParser
  <|> variableTokenParser
  -- <|> parenthesesParser

variableTokenParser :: Text.Parsec.Parsec String () Expression
variableTokenParser = do
  varName <- many1 alphaNum
  skipMany endOfLine
  string " " <|> string ";"
  spaces <|> skipMany endOfLine
  return $ Token $ Variable varName ""

andParser :: Text.Parsec.Parsec String () Expression
andParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "&&"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop And expr1 expr2)

orParser :: Text.Parsec.Parsec String () Expression
orParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "||"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Or expr1 expr2)

equalParser :: Text.Parsec.Parsec String () Expression
equalParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "=="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Equal expr1 expr2)

notEqualParser :: Text.Parsec.Parsec String () Expression
notEqualParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "!="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop NotEqual expr1 expr2)

lessThanParser :: Text.Parsec.Parsec String () Expression
lessThanParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "<"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop LessThan expr1 expr2)

lessThanOrEqualParser :: Text.Parsec.Parsec String () Expression
lessThanOrEqualParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "<="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop LessThanOrEqual expr1 expr2)

greaterThanParser :: Text.Parsec.Parsec String () Expression
greaterThanParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string ">"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop GreaterThan expr1 expr2)

greaterThanOrEqualParser :: Text.Parsec.Parsec String () Expression
greaterThanOrEqualParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string ">="
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop GreaterThanOrEqual expr1 expr2)

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

moduloParser :: Text.Parsec.Parsec String () Expression
moduloParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "%"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Modulo expr1 expr2)

bitwiseAndParser :: Text.Parsec.Parsec String () Expression
bitwiseAndParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "&"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop BitwiseAnd expr1 expr2)

bitwiseOrParser :: Text.Parsec.Parsec String () Expression
bitwiseOrParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "|"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop BitwiseOr expr1 expr2)

bitwiseXorParser :: Text.Parsec.Parsec String () Expression
bitwiseXorParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "^"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop BitwiseXor expr1 expr2)

bitwiseShiftLeftParser :: Text.Parsec.Parsec String () Expression
bitwiseShiftLeftParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "<<"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop BitwiseShiftLeft expr1 expr2)

bitwiseShiftRightParser :: Text.Parsec.Parsec String () Expression
bitwiseShiftRightParser = do
  expr1 <- tokenParser
  spaces <|> skipMany endOfLine
  string ">>"
  spaces <|> skipMany endOfLine
  expr2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop BitwiseShiftRight expr1 expr2)

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
  try helper <|> Uop Return <$> expressionParser
  where
    helper :: Text.Parsec.Parsec String () Expression
    helper = do
      string ";"
      spaces <|> skipMany endOfLine
      return $ Uop Return (Token (Integer 0))

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

subParser :: Text.Parsec.Parsec String () Expression
subParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "-"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Sub num1 num2)

mulParser :: Text.Parsec.Parsec String () Expression
mulParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "*"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Mul num1 num2)

divParser :: Text.Parsec.Parsec String () Expression
divParser = do
  num1 <- tokenParser
  spaces <|> skipMany endOfLine
  string "/"
  spaces <|> skipMany endOfLine
  num2 <- try expressionParser <|> tokenParser
  return $ operatorPrecedenceFix (Bop Div num1 num2)

intLiteralParser :: Text.Parsec.Parsec String () Expression
intLiteralParser = do
  -- many1 digit is consuming the input
  num <- many1 digit
  skipMany endOfLine
  -- notFollowedBy $ string "." <|> string "+"
  string " " <|> string ";"
  spaces <|> skipMany endOfLine
  return $ Token $ Integer (read num :: Integer)

variableParser :: Text.Parsec.Parsec String () Expression
variableParser =
  try variableAssign <|> try variableDeclare <|> try variableAssignAndDeclare
  where variableDeclare :: Text.Parsec.Parsec String () Expression
  -- int a; declare
  -- declareCodeGen with Nothing
        variableDeclare = do
          variableType <- many1 letter
          spaces <|> skipMany endOfLine
          variableName <- many1 letter
          spaces <|> skipMany endOfLine
          string ";"
          spaces <|> skipMany endOfLine
          return $ DeclareVariable variableName variableType Nothing
        variableAssignAndDeclare :: Text.Parsec.Parsec String () Expression
  -- int a = 1; assign and declare
  -- declareCodeGen with (Just value)
        variableAssignAndDeclare = do
          variableType <- many1 letter
          spaces <|> skipMany endOfLine
          variableName <- many1 letter
          spaces <|> skipMany endOfLine
          string "="
          spaces <|> skipMany endOfLine
          value <- try expressionParser <|> tokenParser
          spaces <|> skipMany endOfLine
          return $ DeclareVariable variableName variableType (Just value)
        variableAssign :: Text.Parsec.Parsec String () Expression
  -- a = 1; assign
  -- assignCodeGen
        variableAssign = do
          variableName <- many1 letter
          spaces <|> skipMany endOfLine
          string "="
          spaces <|> skipMany endOfLine
          value <- try expressionParser <|> tokenParser
          spaces <|> skipMany endOfLine
          return $ AssignVariable variableName value

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
  Token . Char <$> anyToken
