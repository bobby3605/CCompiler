module InputLexer where
import Text.Parsec
import Symbol

lexInput :: String -> Either ParseError [Either Symbol String]
lexInput input =
  case parse (many (try symbolLexer)) "" input of
    Left a -> Left a
    Right a -> Right (map wordToSymbol a)
  
symbolLexer :: Text.Parsec.Parsec String () String 
symbolLexer = do
  spaces
  choice [many1 alphaNum
         ,string ";" 
         ,string "{" 
         ,string "}" 
         ,string "(" 
         ,string ")"
         ]
  
wordToSymbol :: String -> Either Symbol String 
wordToSymbol "{" = Left OpenBrace
wordToSymbol "}" = Left CloseBrace
wordToSymbol "(" = Left OpenParenthesis
wordToSymbol ")" = Left CloseParenthesis
wordToSymbol ";" = Left Semicolon
wordToSymbol "int" = Left Int
wordToSymbol "float" = Left Float
wordToSymbol "double" = Left Double
wordToSymbol "return" = Left Return
wordToSymbol a = Right a
