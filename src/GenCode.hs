module GenCode where
import Parsers

returnCodeGen :: Expression -> String
returnCodeGen (Return expr) = case matchCodeGen expr of
  -- A right string means it is a simple token
  Right s -> genmov ("$"++s) "rax"++beginspaces++"ret"++"\n"
  -- Left means it is an expression like Add
  -- it handles eax on its own
  Left s -> s++beginspaces++"ret"++"\n"
returnCodeGen a = error "returnCodeGen pattern failed: "++show a

addCodeGen :: Expression -> Expression -> String
addCodeGen e1 e2 =
  helper e1
  ++beginspaces++"push %rax"++"\n"
  ++helper e2
  ++beginspaces++"pop %rcx"++"\n"
  ++beginspaces++"add %rcx, %rax"++"\n"
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> genmov ("$"++s) "rax"
          Left s -> s

matchCodeGen :: Expression -> Either String String
matchCodeGen (Return expr) = Right $ returnCodeGen $ Return expr
matchCodeGen (Integer a) = Right $ show a
matchCodeGen (Add e1 e2) = Left $ addCodeGen e1 e2

generate :: Program -> String
generate = concatMap functionGenerator

functionGenerator :: Function -> String
functionGenerator (Function returnType name arguments body) =
    ".globl "++name++"\n"
  ++name++":"++"\n"
  ++concatMap helper body
  where helper :: Expression -> String
        helper expr = case matchCodeGen expr of
          Right s -> s
          Left e -> error "functionGenerator received Left " ++ show e

beginspaces :: String
beginspaces = "         "

genmov :: String -> String -> String
genmov val reg = beginspaces++"mov"++"    "++val++", "++"%"++reg++"\n"
